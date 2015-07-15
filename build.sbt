organization := "com.milessabin"

name := "kittens"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

addCompilerPlugin(
  "org.spire-math"  %% "kind-projector" % "0.6.0"
)

libraryDependencies ++= Seq(
  "org.spire-math"  %% "cats"           % "0.1.0-SNAPSHOT" changing(),
  "org.spire-math"  %% "algebra"        % "0.2.0-SNAPSHOT" changing(),
  "com.chuusai"     %% "shapeless"      % "2.2.4",
  "org.scalatest"   %% "scalatest"      % "2.1.3"  % "test",
  "org.scalacheck"  %% "scalacheck"     % "1.12.4" % "test",
  "org.typelevel"   %% "discipline"     % "0.2.1"  % "test"
)

initialCommands in console := """import shapeless._, cats._, cats.derived._"""

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)
