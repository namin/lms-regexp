name := "lms-regexp"

version := "0.2"

organization := "EPFL"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.10.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.10.2"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "org.scala-lang.virtualized" % "scala-actors" % "2.10.2-RC1" % "test"

scalacOptions += "-Yvirtualize"

scalacOptions += "-deprecation"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.2")

scalacOptions += "-P:continuations:enable"
