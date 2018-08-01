import sbt.url

organization := "io.github.definiti"

name := "scala-tests"

scalaVersion := "2.12.6"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "io.github.definiti" %% "core" % "0.3.0"
libraryDependencies += "io.github.definiti" %% "tests" % "0.3.0"
libraryDependencies += "io.github.definiti" %% "scala-model" % "0.3.0"
libraryDependencies += "org.antlr" % "antlr4" % "4.7.1"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "io.github.definiti" % "api" % "0.3.0" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

releasePublishArtifactsAction := PgpKeys.publishSigned.value

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://definiti.gitbook.io/definiti"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/definiti/definiti-scala-tests"),
    "scm:git@github.com:definiti/definiti-scala-tests.git"
  )
)

developers := List(
  Developer(
    id = "grizio",
    name = "Gaëtan Rizio",
    email = "gaetan@rizio.fr",
    url = url("https://github.com/grizio")
  )
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}