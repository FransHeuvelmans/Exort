name := "Exort"
version := "0.2.1"
organization := "dev.hillman"

scalaVersion := "2.13.1"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "com.univocity" % "univocity-parsers" % "2.9.1"

Test / parallelExecution := false
enablePlugins(JavaAppPackaging)