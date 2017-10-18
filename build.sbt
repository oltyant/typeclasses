name := "typeclasses"
organization := "org.laziness"
version := "1.0.0"

scalaVersion := "2.12.3"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.0.0-MF"
)
