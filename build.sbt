import AssemblyKeys._

name := "spellchecker"

version := "1.0.0"

scalaVersion := "2.10.1"

assemblySettings

jarName in assembly := "spellchecker.jar"


scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"
