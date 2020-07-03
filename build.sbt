name := "Exort"

version := "0.2.0"
organization := "dev.hillman"

scalaVersion := "2.13.1"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "com.univocity" % "univocity-parsers" % "2.8.4"

Test / parallelExecution := false