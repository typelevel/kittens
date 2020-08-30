import sbt._

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq("2.12.12", scalaVersion.value)
)

val catsVersion = "2.2.0"
val shapelessVersion = "2.3.3"
val testKitVersion = "2.0.0"

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
    "org.typelevel"   %%% "cats-core"      % catsVersion,
    "org.typelevel"   %%% "alleycats-core" % catsVersion,
    "com.chuusai"     %%% "shapeless"      % shapelessVersion,
    "org.typelevel"   %%% "cats-testkit-scalatest" % testKitVersion % Test,
    compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
  ),
  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/typelevel/kittens"),
      "scm:git:git@github.com:typelevel/kittens.git"
    )),
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

lazy val root = project.in(file("."))
  .aggregate(coreJS, coreJVM)
  .dependsOn(coreJS, coreJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .settings(moduleName := "kittens")
  .settings(coreSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)


lazy val coreJVM = core.jvm
lazy val coreJS = core.js

addCommandAlias("validate", ";root;clean;test;mima")
addCommandAlias("releaseAll", ";root;release")
addCommandAlias("js", ";project coreJS")
addCommandAlias("jvm", ";project coreJVM")
addCommandAlias("root", ";project root")
addCommandAlias("mima", "coreJVM/mimaReportBinaryIssues")

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
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
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
  )
)

lazy val noPublishSettings =
  skip in publish := true
