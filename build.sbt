name := "functional-scala"

version := "0.1"

scalaVersion := "2.13.6"

idePackagePrefix := Some("org.okarmus")


libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test