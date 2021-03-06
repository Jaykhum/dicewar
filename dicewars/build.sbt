import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "dicewars"

version := "1.0"

scalaVersion :=  "2.10.2"


libraryDependencies ++= Seq(
		"org.scala-lang" % "scala-swing" % "2.10.2",
		"org.specs2" % "specs2_2.10" % "2.2.3" % "test",
		"junit" % "junit" % "4.11" % "test",
		"ch.qos.logback" % "logback-classic" % "1.0.9"
)

jacoco.settings