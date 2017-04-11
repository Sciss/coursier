package coursier.maven

import coursier.core._

object Pom {
  import coursier.util.Xml._

  /**
    * Returns either a property's key-value pair or an error if the elem is not an element.
    *
    * This method trims all spaces, whereas Maven has an option to preserve them.
    *
    * @param elem a property element
    * @return the key and the value of the property
    * @see [[https://issues.apache.org/jira/browse/MNG-5380]]
    */
  def property(elem: Node): Either[String, (String, String)] = {
    // Not matching with Text, which fails on scala-js if the property value has xml comments
    if (elem.isElement) Right(elem.label -> elem.textContent.trim)
    else Left(s"Can't parse property $elem")
  }

  // TODO Allow no version in some contexts
  private def module(node: Node, groupIdIsOptional: Boolean = false): Either[String, Module] = {
    for {
      organization <- {
        val e = text(node, "groupId", "Organization")
        if (groupIdIsOptional) e.left.flatMap(_ => Right(""))
        else e
      } .right
      name <- text(node, "artifactId", "Name").right
    } yield Module(organization, name, Map.empty).trim
  }

  private def readVersion(node: Node): String =
    textOpt(node, "version", "Version").getOrElse("").trim

  private implicit final class TraverseU[A](private val seq: Seq[A]) extends AnyVal {
    def traverseU[B, C](fun: A => Either[B, C]): Either[B, Seq[C]] = ???
  }

  def dependency(node: Node): Either[String, (String, Dependency)] = {
    for {
      mod <- module(node)
      version0      = readVersion(node)
      scopeOpt      = textOpt(node, "scope"      , "")
      typeOpt       = textOpt(node, "type"       , "")
      classifierOpt = textOpt(node, "classifier" , "")
      xmlExclusions: Seq[Node] = node.children
        .find(_.label == "exclusions")
        .map(_.children.filter(_.label == "exclusion"))
        .getOrElse(Seq.empty)
      exclusions: Seq[Module] <- {
        xmlExclusions.toList.traverseU[String, Module](module(_)).right
      }
      optional = textOpt(node, "optional", "").toSeq.contains("true")
    } yield scopeOpt.getOrElse("") -> Dependency(
        mod,
        version0,
        "",
        exclusions.map(mod => (mod.organization, mod.name)).toSet,
        Attributes(typeOpt.getOrElse(""), classifierOpt.getOrElse("")),
        optional,
        transitive = true
      )
  }

  private def profileActivation(node: Node): (Option[Boolean], Activation) = {
    val byDefault =
      textOpt(node, "activeByDefault", "").flatMap{
        case "true" => Some(true)
        case "false" => Some(false)
        case _ => None
      }

    val properties = node.children
      .filter(_.label == "property")
      .flatMap{ p =>
        for{
          name <- textOpt(p, "name", "")
          valueOpt = textOpt(p, "value", "")
        } yield (name, valueOpt)
      }

    val osNodeOpt = node.children.collectFirst { case n if n.label == "os" => n }

    val os = Activation.Os(
      osNodeOpt.flatMap(n => textOpt(n, "arch", "")),
      osNodeOpt.flatMap(n => textOpt(n, "family", "")).toSet,
      osNodeOpt.flatMap(n => textOpt(n, "name", "")),
      osNodeOpt.flatMap(n => textOpt(n, "version", ""))
    )

    val jdk = textOpt(node, "jdk", "").flatMap { s =>
      Parse.versionInterval(s).map(Left(_))
        .orElse(Parse.version(s).map(v => Right(Seq(v))))
    }

    val activation = Activation(properties, os, jdk)

    (byDefault, activation)
  }

  def profile(node: Node): Either[String, Profile] = {
    val id = textOpt(node, "id", "Profile ID").getOrElse("")

    val xmlActivationOpt: Option[Node] = node.children
      .find(_.label == "activation")

    val (activeByDefault, activation) = xmlActivationOpt.fold((Option.empty[Boolean], Activation.empty))(profileActivation)

    val xmlDeps: Seq[Node] = node.children
      .find(_.label == "dependencies")
      .map(_.children.filter(_.label == "dependency"))
      .getOrElse(Seq.empty)

    for {
      deps <- xmlDeps.toList.traverseU[String, (String, Dependency)](dependency) .right

      xmlDepMgmts: Seq[Node] = node.children
        .find(_.label == "dependencyManagement")
        .flatMap(_.children.find(_.label == "dependencies"))
        .map(_.children.filter(_.label == "dependency"))
        .getOrElse(Seq.empty)

      depMgmts <- xmlDepMgmts.toList.traverseU[String, (String, Dependency)](dependency) .right

      xmlProperties: Seq[Node] = node.children
        .find(_.label == "properties")
        .map(_.children.collect{case elem if elem.isElement => elem})
        .getOrElse(Seq.empty)

      properties <- {
        xmlProperties.toList.traverseU[String, (String, String)](property)
      } .right

    } yield Profile(id, activeByDefault, activation, deps, depMgmts, properties.toMap)
  }

  def packagingOpt(pom: Node): Option[String] =
    textOpt(pom, "packaging", "")

  def project(pom: Node): Either[String, Project] = {
    for {
      projModule <- module(pom, groupIdIsOptional = true)
      projVersion = readVersion(pom)

      parentOpt = pom.children
        .find(_.label == "parent")
      parentModuleOpt: Option[Module] <- parentOpt
        .map(module(_).right.map(Some(_)))
        .getOrElse(Right(None))
      parentVersionOpt = parentOpt
        .map(readVersion)

      xmlDeps = pom.children
        .find(_.label == "dependencies")
        .map(_.children.filter(_.label == "dependency"))
        .getOrElse(Seq.empty)
      deps <- xmlDeps.toList.traverseU(dependency)

      xmlDepMgmts = pom.children
        .find(_.label == "dependencyManagement")
        .flatMap(_.children.find(_.label == "dependencies"))
        .map(_.children.filter(_.label == "dependency"))
        .getOrElse(Seq.empty)
      depMgmts <- xmlDepMgmts.toList.traverseU(dependency)

      groupId <- Some(projModule.organization).filter(_.nonEmpty)
        .orElse(parentModuleOpt.map(_.organization).filter(_.nonEmpty))
        .toRightDisjunction("No organization found")
      version <- Some(projVersion).filter(_.nonEmpty)
        .orElse(parentVersionOpt.filter(_.nonEmpty))
        .toRightDisjunction("No version found")

      _ <- parentVersionOpt
        .map(v => if (v.isEmpty) Left("Parent version missing") else Right(()))
        .getOrElse(Right(()))
      _ <- parentModuleOpt
        .map(mod => if (mod.organization.isEmpty) Left("Parent organization missing") else Right(()))
        .getOrElse(Right(()))

      xmlProperties = pom.children
        .find(_.label == "properties")
        .map(_.children.collect{case elem if elem.isElement => elem})
        .getOrElse(Seq.empty)
      properties <- xmlProperties.toList.traverseU(property)

      xmlProfiles = pom.children
        .find(_.label == "profiles")
        .map(_.children.filter(_.label == "profile"))
        .getOrElse(Seq.empty)
      profiles <- xmlProfiles.toList.traverseU(profile)

      extraAttrs <- properties
        .collectFirst { case ("extraDependencyAttributes", s) => extraAttributes(s) }
        .getOrElse(Right(Map.empty))

      extraAttrsMap = extraAttrs.map {
        case (mod, ver) =>
          (mod.copy(attributes = Map.empty), ver) -> mod.attributes
      }.toMap

    } yield {

      val description = pom.children
        .find(_.label == "description")
        .map(_.textContent)
        .getOrElse("")

      val homePage = pom.children
        .find(_.label == "url")
        .map(_.textContent)
        .getOrElse("")

      val licenses = pom.children
        .find(_.label == "licenses")
        .toSeq
        .flatMap(_.children)
        .filter(_.label == "license")
        .flatMap { n =>
          textOpt(n, "name", "License name").map { name =>
            (name, textOpt(n, "url", "License URL"))
          }.toSeq
        }

      val developers = pom.children
        .find(_.label == "developers")
        .toSeq
        .flatMap(_.children)
        .filter(_.label == "developer")
        .map { n =>
          for {
            id <- textOpt(n, "id", "Developer ID")
            name <- textOpt(n, "name", "Developer name")
            url <- textOpt(n, "url", "Developer URL")
          } yield Info.Developer(id, name, url)
        }
        .collect {
          case Right(d) => d
        }

      Project(
        projModule.copy(organization = groupId),
        version,
        deps.map {
          case (config, dep0) =>
            val dep = extraAttrsMap.get(dep0.moduleVersion).fold(dep0)(attrs =>
              dep0.copy(module = dep0.module.copy(attributes = attrs))
            )
            config -> dep
        },
        Map.empty,
        parentModuleOpt.map((_, parentVersionOpt.getOrElse(""))),
        depMgmts,
        properties,
        profiles,
        None,
        None,
        packagingOpt(pom),
        None,
        Nil,
        Info(
          description,
          homePage,
          licenses,
          developers,
          None
        )
      )
    }
  }

  def versions(node: Node): Either[String, Versions] = {
    for {
      organization <- textOpt(node, "groupId", "Organization") // Ignored
      name <- textOpt(node, "artifactId", "Name") // Ignored

      xmlVersioning <- node.children
        .find(_.label == "versioning")
        .toRightDisjunction("Versioning info not found in metadata")

      latest = textOpt(xmlVersioning, "latest", "Latest version")
        .getOrElse("")
      release = textOpt(xmlVersioning, "release", "Release version")
        .getOrElse("")

      versionsOpt = xmlVersioning.children
        .find(_.label == "versions")
        .map(_.children.filter(_.label == "version").flatMap(_.children.collectFirst{case Text(t) => t}))

      lastUpdatedOpt = textOpt(xmlVersioning, "lastUpdated", "Last update date and time")
        .flatMap(parseDateTime)

    } yield Versions(latest, release, versionsOpt.map(_.toList).getOrElse(Nil), lastUpdatedOpt)
  }

  def snapshotVersion(node: Node): Either[String, SnapshotVersion] = {
    def textOrEmpty(name: String, desc: String) =
      textOpt(node, name, desc)
        .getOrElse("")

    val classifier = textOrEmpty("classifier", "Classifier")
    val ext = textOrEmpty("extension", "Extensions")
    val value = textOrEmpty("value", "Value")

    val updatedOpt = textOpt(node, "updated", "Updated")
      .flatMap(parseDateTime)

    Right(SnapshotVersion(
      classifier,
      ext,
      value,
      updatedOpt
    ))
  }

  def snapshotVersioning(node: Node): Either[String, SnapshotVersioning] = {
    // FIXME Quite similar to Versions above
    for {
      organization <- textOpt(node, "groupId", "Organization")
      name <- textOpt(node, "artifactId", "Name")
      version = readVersion(node)

      xmlVersioning <- node.children
        .find(_.label == "versioning")
        .toRightDisjunction("Versioning info not found in metadata")

      latest = textOpt(xmlVersioning, "latest", "Latest version")
        .getOrElse("")
      release = textOpt(xmlVersioning, "release", "Release version")
        .getOrElse("")

      versionsOpt = xmlVersioning.children
        .find(_.label == "versions")
        .map(_.children.filter(_.label == "version").flatMap(_.children.collectFirst{case Text(t) => t}))

      lastUpdatedOpt = textOpt(xmlVersioning, "lastUpdated", "Last update date and time")
        .flatMap(parseDateTime)

      xmlSnapshotOpt = xmlVersioning.children
        .find(_.label == "snapshot")

      timestamp = xmlSnapshotOpt
        .flatMap(
          textOpt(_, "timestamp", "Snapshot timestamp")
        )
        .getOrElse("")

      buildNumber = xmlSnapshotOpt
        .flatMap(
          textOpt(_, "buildNumber", "Snapshot build number")
        )
        .filter(s => s.nonEmpty && s.forall(_.isDigit))
        .map(_.toInt)

      localCopy = xmlSnapshotOpt
        .flatMap(
          textOpt(_, "localCopy", "Snapshot local copy")
        )
        .collect{
          case "true" => true
          case "false" => false
        }

      xmlSnapshotVersions = xmlVersioning.children
        .find(_.label == "snapshotVersions")
        .map(_.children.filter(_.label == "snapshotVersion"))
        .getOrElse(Seq.empty)
      snapshotVersions <- xmlSnapshotVersions
        .toList
        .traverseU(snapshotVersion)
    } yield SnapshotVersioning(
      Module(organization, name, Map.empty),
      version,
      latest,
      release,
      timestamp,
      buildNumber,
      localCopy,
      lastUpdatedOpt,
      snapshotVersions
    )
  }

  val extraAttributeSeparator = ":#@#:"
  val extraAttributePrefix = "+"

  val extraAttributeOrg = "organisation"
  val extraAttributeName = "module"
  val extraAttributeVersion = "revision"

  val extraAttributeBase = Set(
    extraAttributeOrg,
    extraAttributeName,
    extraAttributeVersion,
    "branch"
  )

  val extraAttributeDropPrefix = "e:"

  def extraAttribute(s: String): Either[String, (Module, String)] = {
    // vaguely does the same as:
    // https://github.com/apache/ant-ivy/blob/2.2.0/src/java/org/apache/ivy/core/module/id/ModuleRevisionId.java#L291

    // dropping the attributes with a value of NULL here...

    val rawParts = s.split(extraAttributeSeparator).toSeq

    val partsOrError =
      if (rawParts.length % 2 == 0) {
        val malformed = rawParts.filter(!_.startsWith(extraAttributePrefix))
        if (malformed.isEmpty)
          Right(rawParts.map(_.drop(extraAttributePrefix.length)))
        else
          Left(s"Malformed attributes ${malformed.map("'"+_+"'").mkString(", ")} in extra attributes '$s'")
      } else
        Left(s"Malformed extra attributes '$s'")

    def attrFrom(attrs: Map[String, String], name: String): Either[String, String] =
      attrs.get(name)
        .toRight(s"$name not found in extra attributes '$s'")

    for {
      parts <- partsOrError
      attrs = parts.grouped(2).collect {
        case Seq(k, v) if v != "NULL" =>
          k.stripPrefix(extraAttributeDropPrefix) -> v
      }.toMap
      org     <- attrFrom(attrs, extraAttributeOrg).right
      name    <- attrFrom(attrs, extraAttributeName).right
      version <- attrFrom(attrs, extraAttributeVersion).right
      remainingAttrs = attrs.filterKeys(!extraAttributeBase(_))
    } yield (Module(org, name, remainingAttrs.toVector.toMap), version)
  }

  def extraAttributes(s: String): Either[String, Seq[(Module, String)]] = {
    val lines = s.split('\n').toSeq.map(_.trim).filter(_.nonEmpty)

    lines.foldLeft[Either[String, Seq[(Module, String)]]](Right(Vector.empty)) {
      case (acc, line) =>
        for {
          modVers <- acc.right
          modVer <- extraAttribute(line)
        } yield modVers :+ modVer
    }
  }

  def addOptionalDependenciesInConfig(
    proj: Project,
    fromConfigs: Set[String],
    optionalConfig: String
  ): Project = {

    val optionalDeps = proj.dependencies.collect {
      case (conf, dep) if dep.optional && fromConfigs(conf) =>
        optionalConfig -> dep.copy(optional = false)
    }

    val configurations = proj.configurations +
      (optionalConfig -> (proj.configurations.getOrElse(optionalConfig, Nil) ++ fromConfigs.filter(_.nonEmpty)).distinct)

    proj.copy(
      configurations = configurations,
      dependencies = proj.dependencies ++ optionalDeps
    )
  }
}