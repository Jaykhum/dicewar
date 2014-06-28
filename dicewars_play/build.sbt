name := "dicewars"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.scala-lang" % "scala-swing" % "2.10.2",
  "org.specs2" %% "specs2" % "2.3.12",
  "ch.qos.logback" % "logback-classic" % "1.0.9"
)     

play.Project.playScalaSettings
