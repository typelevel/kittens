import sbt.*

val scala212 = "2.12.18"
val scala213 = "2.13.11"
val scala3 = "3.3.1"

ThisBuild / crossScalaVersions := Seq(scala212, scala213, scala3)
ThisBuild / scalaVersion := scala3
ThisBuild / tlBaseVersion := "3.0"
ThisBuild / organization := "org.typelevel"

val catsVersion = "2.10.0"
val munitVersion = "1.0.0-M8"
val disciplineMunitVersion = "2.0.0-M3"
val kindProjectorVersion = "0.13.2"
val shapeless2Version = "2.3.10"
val shapeless3Version = "3.3.0"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation",
    "-Xfatal-warnings"
  ),
  scalacOptions ++= CrossVersion.partialVersion(scalaVersion.value).toList.flatMap {
    case (3, _) => List("-Xmax-inlines", "64")
    case (2, 12) => List("-Ypartial-unification")
    case _ => Nil
  },
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.typelevel" %%% "alleycats-core" % catsVersion,
    "org.typelevel" %%% "cats-testkit" % catsVersion % Test,
    "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test,
    "org.scalameta" %%% "munit" % munitVersion % Test
  ),
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq("org.typelevel" %%% "shapeless3-deriving" % shapeless3Version)
    case _ =>
      Seq(
        "com.chuusai" %%% "shapeless" % shapeless2Version,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test,
        compilerPlugin(("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full))
      )
  }),
  Test / parallelExecution := false
)

console / initialCommands := """import shapeless._, cats._, cats.derived._"""

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "kittens")
  .settings(commonSettings *)
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
  Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr")),
  Developer("TimWSpence", "Tim Spence", "timothywspence@gmail.com", url("https://twitter.com/timwspence"))
)

ThisBuild / tlCiScalafmtCheck := true
ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / mergifyStewardConfig := Some(
  MergifyStewardConfig(
    author = "typelevel-steward[bot]",
    mergeMinors = true
  )
)
