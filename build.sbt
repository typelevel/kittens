import sbt._

val scala212 = "2.12.13"
val scala213 = "2.13.6"
val scala3 = "3.0.0"

ThisBuild / crossScalaVersions := Seq(scala212, scala213, scala3)
ThisBuild / scalaVersion := scala3

lazy val buildSettings = Seq(
  organization := "org.typelevel"
)

val catsVersion = "2.6.1"
val disciplineMunitVersion = "1.0.9"
val kindProjectorVersion = "0.13.0"
val shapeless2Version = "2.3.7"
val shapeless3Version = "3.0.0"

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation",
    "-Xfatal-warnings"
  ),
  Test / scalacOptions += "-Xprint:inline",
  scalacOptions ++= CrossVersion.partialVersion(scalaVersion.value).flatMap {
    case (2, 12) => Some("-Ypartial-unification")
    case _ => None
  },
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "org.typelevel" %%% "alleycats-core" % catsVersion,
    "org.typelevel" %%% "cats-testkit" % catsVersion % Test,
    "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test
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
  Test / parallelExecution := false,
  versionScheme := Some("semver-spec"),
  mimaPreviousArtifacts := Set(organization.value %% moduleName.value % "2.2.1")
)

console / initialCommands := """import shapeless._, cats._, cats.derived._"""

lazy val commonJsSettings = Seq(
  Global / scalaJSStage := FastOptStage,
  Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

lazy val coreSettings =
  Seq.concat(buildSettings, commonSettings, crossVersionSharedSources, publishSettings)

lazy val kittens = project
  .in(file("."))
  .aggregate(coreJVM)
  .dependsOn(coreJVM)
  .settings(coreSettings: _*)
  .settings(noPublishSettings)

lazy val core = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "kittens")
  .settings(coreSettings: _*)

lazy val coreJVM = core.jvm

addCommandAlias("root", ";project kittens")
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

lazy val crossVersionSharedSources: Seq[Setting[_]] = Seq(Compile, Test).map { sc =>
  (sc / unmanagedSourceDirectories) ++= (sc / unmanagedSourceDirectories).value.map { dir: File =>
    new File(dir.getPath + "-" + scalaBinaryVersion.value.head)
  }
}

lazy val publishSettings = Seq(
  Test / publishArtifact := false,
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
  publish / skip := true

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixAdditions += "ci" -> List("test")
ThisBuild / githubWorkflowBuild := List(WorkflowStep.Sbt(List("${{ matrix.ci }}"), name = Some("Validation")))
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
