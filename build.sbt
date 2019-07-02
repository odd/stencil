val Stencil = "0.9.1-SNAPSHOT"
val Scala = "2.12.8"
val ScalaXml = "1.2.0"
val ScalaTest = "3.0.7"
val uJson = "0.7.4"

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
