name := "stencil"

organization := "org.bitbonanza"

version := "0.3-SNAPSHOT"

crossScalaVersions := Seq("2.9.2", "2.10.1")

resolvers += Classpaths.typesafeResolver

resolvers += Classpaths.typesafeSnapshots

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies <++= (scalaVersion) {
  case "2.10.1" => Seq("org.scalatest" %% "scalatest" % "1.9.1" % "test" withSources)
  case _ => Seq("org.scalatest" %% "scalatest" % "1.8" % "test" withSources)
}
