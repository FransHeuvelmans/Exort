name := "Exort"

version := "0.1.0"

scalaVersion := "2.13.1"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "com.univocity" % "univocity-parsers" % "2.8.3"