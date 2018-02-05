name := "lms-regexp-benchmarks"

version := "0.2"

organization := "EPFL"

scalaVersion := "2.11.12"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

libraryDependencies += "dk.brics.automaton" % "automaton" % "1.11-8"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint")

publishArtifact := false

parallelExecution := false

logBuffered := false
