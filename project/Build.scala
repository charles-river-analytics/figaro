import sbt._
import Keys._

object FigaroBuild extends Build {

  val _scalaVersion = "2.10.1"

  lazy val root = Project("root", file("."))
    .dependsOn(figaro, examples)
    .aggregate(figaro, examples)

  lazy val figaro = Project("figaro", file("Figaro"))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation"
    ))
    .settings(version := "2.2.0.0")
    .settings(scalaVersion := _scalaVersion)
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-actors" % _scalaVersion,
      "org.scala-lang" % "scala-reflect" % _scalaVersion,
      "asm" % "asm" % "3.3.1",
      "net.sf.jsci" % "jsci" % "1.2",
      "org.scalatest" %% "scalatest" % "2.0" % "test"
    ))
    .settings(parallelExecution in Test := false)
  
  lazy val examples = Project("figaro-examples", file("FigaroExamples"))
    .settings(version := "2.2.0.0")
    .dependsOn(figaro)

}
