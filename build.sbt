val Stencil = "0.9.0-SNAPSHOT"
val Scala = "2.12.7"
val ScalaXml = "1.1.0"
val ScalaTest = "3.0.5"
val uJson = "0.6.6"

ThisBuild / organization := "org.bitbonanza"
ThisBuild / version      := Stencil
ThisBuild / scalaVersion := Scala
ThisBuild / resolvers    ++= Seq(
  Resolver.defaultLocal,
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayRepo("sbt", "sbt-plugin-releases"),
  "Akka Snapshots" at "https://repo.akka.io/snapshots/")

lazy val root = (project in file("."))
  .settings(
    name := "stencil",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % ScalaTest % Test withSources(),
      "org.scala-lang.modules" %% "scala-xml" % ScalaXml withSources(),
      "com.lihaoyi" %% "ujson" % uJson withSources()))