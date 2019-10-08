val Stencil = "1.0.0"
val Scala = "2.13.1"
val ScalaXml = "1.2.0"
val ScalaTest = "3.0.8"
val uJson = "0.8.0"

organization := "org.bitbonanza"
version      := Stencil
scalaVersion := Scala
resolvers    ++= Seq(
  Resolver.defaultLocal,
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayRepo("sbt", "sbt-plugin-releases"),
  "Akka Snapshots" at "https://repo.akka.io/snapshots/")

lazy val stencil = (project in file("."))
  .settings(
    name := "stencil",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % ScalaTest % Test withSources(),
      "org.scala-lang.modules" %% "scala-xml" % ScalaXml withSources(),
      "com.lihaoyi" %% "ujson" % uJson withSources()))
