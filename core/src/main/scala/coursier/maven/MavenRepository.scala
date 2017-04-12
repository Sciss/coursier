package coursier.maven

import coursier.Fetch
import coursier.core._
import coursier.core.compatibility.encodeURIComponent
import coursier.util.WebPage

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object MavenRepository {

  def ivyLikePath(
    org       : String,
    dirName   : String,
    name      : String,
    version   : String,
    subDir    : String,
    baseSuffix: String,
    ext       : String
  ): Seq[String] =
    Seq(
      org,
      dirName,
      version,
      subDir,
      s"$name$baseSuffix.$ext"
    )

  def mavenVersioning(
    snapshotVersioning: SnapshotVersioning,
    classifier        : String,
    extension         : String
  ): Option[String] =
    snapshotVersioning
      .snapshotVersions
      .find(v => v.classifier == classifier && v.extension == extension)
      .map(_.value)
      .filter(_.nonEmpty)


  val defaultConfigurations: Map[String, Seq[String]] = Map(
    "compile" -> Seq.empty,
    "runtime" -> Seq("compile"),
    "default" -> Seq("runtime"),
    "test"    -> Seq("runtime")
  )

  def dirModuleName(module: Module, sbtAttrStub: Boolean): String =
    if (sbtAttrStub) {
      var name = module.name
      for (scalaVersion <- module.attributes.get("scalaVersion"))
        name = name + "_" + scalaVersion
      for (sbtVersion <- module.attributes.get("sbtVersion"))
        name = name + "_" + sbtVersion
      name
    } else
      module.name

}

final case class MavenRepository(
  root: String,
  changing: Option[Boolean] = None,
  /** Hackish hack for sbt plugins mainly - what this does really sucks */
  sbtAttrStub: Boolean = true,
  authentication: Option[Authentication] = None
) extends Repository {

  import Repository._
  import MavenRepository._

  val root0: String = if (root.endsWith("/")) root else root + "/"
  val source = MavenSource(root0, changing, sbtAttrStub, authentication)

  private def modulePath(
    module: Module,
    version: String
  ): Seq[String] =
    module.organization.split('.').toSeq ++ Seq(
      dirModuleName(module, sbtAttrStub),
      version
    )

  private def urlFor(path: Seq[String]): String =
    root0 + path.map(encodeURIComponent).mkString("/")

  def projectArtifact(
    module          : Module,
    version         : String,
    versioningValue : Option[String]
  ): Artifact = {

    val path = modulePath(module, version) :+
      s"${module.name}-${versioningValue getOrElse version}.pom"

    Artifact(
      urlFor(path),
      Map.empty,
      Map.empty,
      Attributes("pom", ""),
      changing = changing.getOrElse(version.contains("-SNAPSHOT")),
      authentication = authentication
    )
    .withDefaultChecksums
    .withDefaultSignature
  }

  def versionsArtifact(module: Module): Option[Artifact] = {

    val path = module.organization.split('.').toSeq ++ Seq(
      dirModuleName(module, sbtAttrStub),
      "maven-metadata.xml"
    )

    val artifact =
      Artifact(
        urlFor(path),
        Map.empty,
        Map.empty,
        Attributes("pom", ""),
        changing = true,
        authentication = authentication
      )
      .withDefaultChecksums
      .withDefaultSignature

    Some(artifact)
  }

  def snapshotVersioningArtifact(
    module: Module,
    version: String
  ): Option[Artifact] = {

    val path = modulePath(module, version) :+ "maven-metadata.xml"

    val artifact =
      Artifact(
        urlFor(path),
        Map.empty,
        Map.empty,
        Attributes("pom", ""),
        changing = true,
        authentication = authentication
      )
      .withDefaultChecksums
      .withDefaultSignature

    Some(artifact)
  }

  def versions(
    module: Module,
    fetch: Fetch.Content
  )(implicit exec: ExecutionContext): Future[Either[String, Versions]] =
      versionsArtifact(module) match {
        case None => Future.successful(Left("Not supported"))
        case Some(artifact) =>
          fetch(artifact).map(eitherStr =>
            for {
              str <- eitherStr.right
              xml <- compatibility.xmlParse(str).right
              _ <- (if (xml.label == "metadata") Right(()) else Left("Metadata not found")).right
              versions <- Pom.versions(xml).right
            } yield versions
          )
      }

  def snapshotVersioning(
    module: Module,
    version: String,
    fetch: Fetch.Content
  )(implicit exec: ExecutionContext): Future[Either[String, SnapshotVersioning]] = {
      snapshotVersioningArtifact(module, version) match {
        case None => Future.successful(Left("Not supported"))
        case Some(artifact) =>
          fetch(artifact).map(eitherStr =>
            for {
              str <- eitherStr.right
              xml <- compatibility.xmlParse(str).right
              _ <- (if (xml.label == "metadata") Right(()) else Left("Metadata not found")).right
              snapshotVersioning <- Pom.snapshotVersioning(xml).right
            } yield snapshotVersioning
          )
      }
  }

  def findNoInterval(
    module: Module,
    version: String,
    fetch: Fetch.Content
  )(implicit exec: ExecutionContext): Future[Either[String, Project]] = {
      def withSnapshotVersioning: Future[Either[String, Project]] =
        snapshotVersioning(module, version, fetch).flatMap[Either[String, Project]] { svE =>
          svE.fold[Future[Either[String, Project]]](err => Future.successful(Left(err)), { sv =>
            val versioningOption: Option[String] =
              mavenVersioning(sv, "", "jar")
                .orElse(mavenVersioning(sv, "", ""))

            val res: Future[Either[String, Project]] = versioningOption match {
              case None =>
                Future.successful[Either[String, Project]](Left("No snapshot versioning value found"))
              case versioning @ Some(_) =>
                findVersioning(module, version, versioning, fetch)
                  .map[Either[String, Project]](_.right.map(_.copy(snapshotVersioning = Some(sv))))
            }
            res
          })
        }

      val res = findVersioning(module, version, None, fetch).flatMap { eitherProj =>
        if (eitherProj.isLeft && version.contains("-SNAPSHOT"))
          withSnapshotVersioning.map(eitherProj0 =>
            if (eitherProj0.isLeft)
              eitherProj
            else
              eitherProj0
          )
        else
          Future.successful(eitherProj)
      }

      // keep exact version used to get metadata, in case the one inside the metadata is wrong
      res.map(_.right.map(proj => proj.copy(actualVersionOpt = Some(version))))
    }

  type VerContent = Either[String, Project]
  type VerResult  = Future[VerContent]

  def findVersioning(
    module: Module,
    version: String,
    versioningValue: Option[String],
    fetch: Fetch.Content
  )(implicit exec: ExecutionContext): VerResult = {

    def parseRawPom(str: String): Either[String, Project] =
      for {
        xml <- compatibility.xmlParse(str).right
        _   <- (if (xml.label == "project") Right(()) else Left("Project definition not found")).right
        proj <- Pom.project(xml).right
      } yield proj

    def artifactFor(url: String): Artifact =
      Artifact(
        url,
        Map.empty,
        Map.empty,
        Attributes("", ""),
        changing = changing.getOrElse(version.contains("-SNAPSHOT")),
        authentication
      )

    def isArtifact(fileName: String, prefix: String): Option[(String, String)] =
      // TODO There should be a regex for that...
      if (fileName.startsWith(prefix)) {
        val end = fileName.stripPrefix(prefix)
        val idx = end.indexOf('.')
        if (idx >= 0) {
          val ext = end.drop(idx + 1)
          val rem = end.take(idx)
          if (rem.isEmpty)
            Some(("", ext))
          else if (rem.startsWith("-"))
            Some((rem.drop(1), ext))
          else
            None
        } else
          None
      } else
        None


    val listFilesUrl = urlFor(modulePath(module, version)) + "/"

    fetch(projectArtifact(module, version, versioningValue)).flatMap { strE =>
      strE.fold[VerResult](err => Future.successful(Left(err)), { str =>
        fetch(artifactFor(listFilesUrl)).map { e =>
          val rawListFilesPageOpt = e.fold[Option[String]](_ => None, Some(_))
          parseRawPom(str).right.map { proj0 =>
            val foundPublications =
              rawListFilesPageOpt match {
                case Some(rawListFilesPage) =>

                  val files = WebPage.listFiles(listFilesUrl, rawListFilesPage)

                  val prefix = s"${module.name}-${versioningValue.getOrElse(version)}"

                  val packagingTpeMap = proj0.packagingOpt
                    .map { packaging =>
                      (MavenSource.typeDefaultClassifier(packaging), MavenSource.typeExtension(packaging)) -> packaging
                    }
                    .toMap

                  files
                    .flatMap(isArtifact(_, prefix))
                    .map {
                      case (classifier, ext) =>
                        val tpe = packagingTpeMap.getOrElse(
                          (classifier, ext),
                          MavenSource.classifierExtensionDefaultTypeOpt(classifier, ext).getOrElse(ext)
                        )
                        val config = MavenSource.typeDefaultConfig(tpe).getOrElse("compile")
                        config -> Publication(
                          module.name,
                          tpe,
                          ext,
                          classifier
                        )
                    }

                case None =>
                  // Publications can't be listed - MavenSource then handles that
                  Nil
              }

            val proj = Pom.addOptionalDependenciesInConfig(
              proj0.copy(configurations = defaultConfigurations),
              Set("", "default"),
              "optional"
            )

            proj.copy(
              actualVersionOpt = Some(version),
              publications = foundPublications
            )
          }
        }
      })
    }
  }

  def find(
    module: Module,
    version: String,
    fetch: Fetch.Content
  )(implicit exec: ExecutionContext): FindResult = {

    Parse.versionInterval(version)
      .orElse(Parse.ivyLatestSubRevisionInterval(version))
      .filter(_.isValid) match {
        case None =>
          findNoInterval(module, version, fetch).map(_.right.map((source, _)))
        case Some(itv) =>
          versions(module, fetch).flatMap { versions0E =>
            versions0E.fold(err => Future.successful(Left(err)), { versions0 =>
              val eitherVersion = {
                val release = Version(versions0.release)

                if (itv.contains(release)) Right(versions0.release)
                else {
                  val inInterval = versions0.available
                    .map(Version(_))
                    .filter(itv.contains)

                  if (inInterval.isEmpty) Left(s"No version found for $version")
                  else Right(inInterval.max.repr)
                }
              }

              eitherVersion match {
                case Left(reason) => Future.successful(Left(reason))
                case Right(version0) =>
                  findNoInterval(module, version0, fetch)
                    .map(_.right.map(_.copy(versions = Some(versions0))))
                    .map(_.right.map((source, _)))
              }
            })
          }
    }
  }
}