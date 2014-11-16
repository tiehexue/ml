import AssemblyKeys._

name := """DeepLearning"""

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.apache.commons" % "commons-collections4" % "4.0"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

assemblySettings

mainClass in assembly := Some("com.wangyz.Main")


