name := """parsemecker"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.4.0"
