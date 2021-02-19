import sbt._

ThisBuild / crossScalaVersions := Seq("2.12.13", "2.13.4")
ThisBuild / scalaVersion := "2.13.4"

lazy val buildSettings = Seq(
  organization := "org.typelevel"
)

val catsVersion = "2.4.2"
val shapelessVersion = "2.3.3"
val testKitVersion = "2.1.2"

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation",
    "-Xfatal-warnings"
  ),
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 => Seq("-Ypartial-unification")
      case _ => Seq.empty
    }
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.typelevel" %%% "alleycats-core" % catsVersion,
    "com.chuusai" %%% "shapeless" % shapelessVersion,
    "org.typelevel" %%% "cats-testkit-scalatest" % testKitVersion % Test,
    compilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.3").cross(CrossVersion.full))
  ),
  testOptions += Tests.Argument("-oF"),
  mimaPreviousArtifacts := Set(organization.value %% moduleName.value % "2.0.0")
) ++ crossVersionSharedSources

initialCommands in console := """import shapeless._, cats._, cats.derived._"""

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false
)

lazy val coreSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val kittens = project
  .in(file("."))
  .aggregate(coreJS, coreJVM)
  .dependsOn(coreJS, coreJVM)
  .settings(coreSettings: _*)
  .settings(noPublishSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "kittens")
  .settings(coreSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .jvmSettings(commonJvmSettings: _*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

addCommandAlias("root", ";project kittens")
addCommandAlias("jvm", ";project coreJVM")
addCommandAlias("js", ";project coreJS")

addCommandAlias("validate", "all scalafmtCheckAll scalafmtSbtCheck test doc coreJVM/mimaReportBinaryIssues")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("mima", "coreJVM/mimaReportBinaryIssues")

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc).value.map { dir: File =>
        new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val publishSettings = Seq(
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/typelevel/kittens")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/typelevel/kittens"), "scm:git:git@github.com:typelevel/kittens.git")),
  developers := List(
    Developer("milessabin", "Miles Sabin", "", url("http://milessabin.com/blog")),
    Developer("kailuowang", "Kai(luo) Wang", "kailuo.wang@gmail.com", url("http://kailuowang.com/")),
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
  )
)

lazy val noPublishSettings =
  skip in publish := true

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(List("validate"), id = None, name = Some("Build and Validate")))
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v")))
ThisBuild / githubWorkflowPublishPreamble += WorkflowStep.Use(UseRef.Public("olafurpg", "setup-gpg", "v3"))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)
