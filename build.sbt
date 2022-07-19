import sbt._

ThisBuild / crossScalaVersions := Seq("2.12.16", "2.13.8")
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / tlBaseVersion := "2.3"
ThisBuild / organization := "org.typelevel"

val catsVersion = "2.8.0"
val disciplineMunitVersion = "1.0.9"
val kindProjectorVersion = "0.13.2"
val shapelessVersion = "2.3.9"

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
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.typelevel" %%% "alleycats-core" % catsVersion,
    "com.chuusai" %%% "shapeless" % shapelessVersion,
    "org.typelevel" %%% "cats-testkit" % catsVersion % Test,
    "org.scalameta" %%% "munit" % "0.7.29" % Test,
    "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test,
    compilerPlugin(("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full))
  ),
  Test / parallelExecution := false
)

console / initialCommands := """import shapeless._, cats._, cats.derived._"""

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "kittens")
  .settings(commonSettings: _*)
  .jsSettings(tlVersionIntroduced := List("2.12", "2.13").map(_ -> "2.1.0").toMap)
  .nativeSettings(tlVersionIntroduced := List("2.12", "2.13").map(_ -> "2.2.2").toMap)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

addCommandAlias("root", ";project /")
addCommandAlias("jvm", ";project coreJVM")
addCommandAlias("js", ";project coreJS")
addCommandAlias("native", ";project coreNative")

addCommandAlias(
  "validateJVM",
  "all scalafmtCheckAll scalafmtSbtCheck coreJVM/test coreJVM/doc coreJVM/mimaReportBinaryIssues"
)
addCommandAlias("validateJS", "all coreJS/test")
addCommandAlias("validateNative", "all coreNative/test")
addCommandAlias("mima", "coreJVM/mimaReportBinaryIssues")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")

ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  Developer("milessabin", "Miles Sabin", "", url("http://milessabin.com/blog")),
  Developer("kailuowang", "Kai(luo) Wang", "kailuo.wang@gmail.com", url("http://kailuowang.com/")),
  Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
)

ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / mergifyStewardConfig := Some(
  MergifyStewardConfig(
    author = "typelevel-steward[bot]",
    mergeMinors = true
  )
)
ThisBuild / tlCiScalafmtCheck := true
