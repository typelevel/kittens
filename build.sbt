import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.cross.CrossProject
import ReleaseTransformations._

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8")
)

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked"
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "bintray/non" at "http://dl.bintray.com/non/maven"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel"  %% "cats"           % "0.7.2",
    "org.typelevel"   %% "alleycats-core" % "0.1.7",
    "com.chuusai"     %% "shapeless"      % "2.3.2",
    "org.typelevel"   %% "export-hook"    % "1.1.0",
    "org.scalatest"   %% "scalatest"      % "3.0.0-M7" % "test",
    "org.scalacheck"  %% "scalacheck"     % "1.12.5" % "test",
    "org.typelevel"   %% "discipline"     % "0.4" % "test",

    compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.1.0" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
  ),

  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/milessabin/kittens"),
      "scm:git:git@github.com:milessabin/kittens.git"
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
  .aggregate(coreJS, coreJVM, extraTests)
  .dependsOn(coreJS, coreJVM, extraTests)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val core = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "kittens")
  .settings(coreSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

//Monad and Applicative tests are taking a long time to compile, separating them to another module to help development, and scala 2.10.x build on travis.
lazy val extraTests = project.in(file("extra-tests"))
  .settings(moduleName := "kittens-tests")
  .dependsOn(coreJVM % "compile->compile;test->test")
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

addCommandAlias("validate", ";root;compile;test")
addCommandAlias("releaseAll", ";root;release")
addCommandAlias("js", ";project coreJS")
addCommandAlias("jvm", ";project coreJVM")
addCommandAlias("root", ";project root")

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq("org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary)
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
  homepage := Some(url("https://github.com/milessabin/kittens")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
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
