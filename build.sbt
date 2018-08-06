import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.cross.CrossProject
import ReleaseTransformations._
import sbt._
import sbtcrossproject.{CrossType, crossProject}

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq( "2.11.12", scalaVersion.value, "2.13.0-M4")
)

val catsVersion = "1.2.0"

val ScalaTestVersion = Def.setting{
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 12 =>
      "3.0.5"
    case _ =>
      "3.0.6-SNAP1"
  }
}
val ScalaCheckVersion = Def.setting{
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 12 =>
      "1.13.5"
    case _ =>
      "1.14.0"
  }
}
val DisciplineVersion = Def.setting{
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 12 =>
      "0.9.0"
    case _ =>
      "0.10.0"
  }
}

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked"
  ),
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 => Seq(
        "-Ypartial-unification"
      )
      case _ => Seq(
      )
    }
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "bintray/non" at "http://dl.bintray.com/non/maven"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel"   %% "cats-core"      % catsVersion,
    "org.typelevel"   %% "alleycats-core" % catsVersion,
    "com.chuusai"     %% "shapeless"      % "2.3.3",
    "org.scalatest"   %% "scalatest"      % ScalaTestVersion.value % "test",
    "org.scalacheck"  %% "scalacheck"     % ScalaCheckVersion.value % "test",
    "org.typelevel"   %% "cats-laws"      % catsVersion % "test",
    "org.typelevel"   %% "discipline"     % DisciplineVersion.value % "test",
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
  ),
  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/typelevel/kittens"),
      "scm:git:git@github.com:typelevel/kittens.git"
    ))
) ++ crossVersionSharedSources ++ scalaMacroDependencies

initialCommands in console := """import shapeless._, cats._, cats.derived._"""

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false
)

lazy val coreSettings = buildSettings ++ commonSettings ++ publishSettings ++ releaseSettings

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

addCommandAlias("validate", ";root;clean;test")
addCommandAlias("releaseAll", ";root;release")
addCommandAlias("js", ";project coreJS")
addCommandAlias("jvm", ";project coreJVM")
addCommandAlias("root", ";project root")

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided"
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
        )
      case _ =>
        // if scala 2.13.0-M4 or later, macro annotations merged into scala-reflect
        // https://github.com/scala/scala/pull/6606
        Nil
    }
  }
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/typelevel/kittens")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>milessabin</id>
        <name>Miles Sabin</name>
        <url>http://milessabin.com/blog</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
