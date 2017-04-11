parallelExecution.in(Global) := false

scalaVersion in ThisBuild := "2.11.8"

// ---- dependencies ----

val scalazVersion = "7.2.8"

def depQuasiQuotes           = "org.scalamacros"        %% "quasiquotes"        % "2.1.0"
def depFastParse             = "com.lihaoyi"            %% "fastparse"          % "0.4.2"
def depJsoup                 = "org.jsoup"              %  "jsoup"              % "1.10.2"
def depScalaXml              = "org.scala-lang.modules" %% "scala-xml"          % "1.0.6"
def depScalazCore            = "org.scalaz"             %% "scalaz-core"        % scalazVersion
def depScalazConcurrent      = "org.scalaz"             %% "scalaz-concurrent"  % scalazVersion
def depUtest                 = "com.lihaoyi"            %% "utest"              % "0.4.5"

def scalaAsync = Def.setting {
  val version =
    if (scalaBinaryVersion.value == "2.10") "0.9.5"
    else "0.9.6"

  "org.scala-lang.modules" %% "scala-async" % version
}

// ---- CoursierSettings.scala ----

lazy val generatePropertyFile =
  resourceGenerators.in(Compile) += {
    (target, version).map { (dir, ver) =>
      import sys.process._

      val f = dir / "coursier.properties"
      dir.mkdirs()

      val p = new java.util.Properties

      p.setProperty("version", ver)
      p.setProperty("commit-hash", Seq("git", "rev-parse", "HEAD").!!.trim)

      val w = new java.io.FileOutputStream(f)
      p.store(w, "Coursier properties")
      w.close()

      println(s"Wrote $f")

      Seq(f)
    }.taskValue
  }

lazy val coursierPrefix = {
  name := "coursier-" + name.value
}

lazy val scalaXmlIfNecessary = Seq(
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.10") Seq()
    else Seq(depScalaXml)
  }
)

lazy val quasiQuotesIfNecessary = Seq(
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.10")
    // directly depending on that one so that it doesn't get shaded
      Seq(depQuasiQuotes)
    else
      Nil
  }
)

lazy val utest = Seq(
  libraryDependencies += depUtest % "test",
  testFrameworks += new TestFramework("utest.runner.Framework")
)

def renameMainJar(name: String) = {
  artifactName := {
    val artifactName0 = artifactName.value
    (sv, m, artifact) =>
      if (artifact.`type` == "jar" && artifact.extension == "jar")
        name
      else
        artifactName0(sv, m, artifact)
  }
}

/////////////
// modules //
/////////////

lazy val core = project
  .settings(
    name := "coursier",
    quasiQuotesIfNecessary,
    scalaXmlIfNecessary,
    libraryDependencies ++= Seq(
      depFastParse,
      depScalazCore,
      depJsoup
    ),
    generatePropertyFile
  )

lazy val tests = project
  .dependsOn(core)
  .dependsOn(cache % "test")
  .settings(
    coursierPrefix,
    libraryDependencies += scalaAsync.value,
    utest,
    sharedTestResources
  )

lazy val cache = project
  .dependsOn(core)
  .settings(
    coursierPrefix,
    libraryDependencies += depScalazConcurrent
  )

lazy val jvm = project
  .aggregate(
    core,
    tests,
    cache
  )
  .settings(
    moduleName := "coursier-jvm"
  )

lazy val coursier = project
  .in(file("."))
  .aggregate(
    core,
    tests,
    cache
  )
  .settings(
    moduleName := "coursier-root"
  )

lazy val sharedTestResources = {
  unmanagedResourceDirectories.in(Test) += baseDirectory.in(LocalRootProject).value / "tests" / "shared" / "src" / "test" / "resources"
}
