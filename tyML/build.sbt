name := "tyML"

scalaVersion := "2.11.6"

scalacOptions ++= Seq(
  "-feature",
  "-unchecked",
  "-deprecation",
  "-language:implicitConversions",
  "-language:higherKinds")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

