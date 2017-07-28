name := "scala-neophyte"

version := "0.1"

scalaVersion := "2.12.2"

scalacOptions += "-feature"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// for Part12
libraryDependencies += "joda-time" % "joda-time" % "2.9.9"
libraryDependencies += "org.joda" % "joda-convert" % "1.8.2"

// for Part13
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.3"