import sbt._
import Keys._

object FigaroBuild extends Build {

  override val settings = super.settings ++ Seq(
    organization := "com.cra.figaro",
    description := "Figaro: a language for probablistic programming",
    version := "2.2.0.0",
    scalaVersion := "2.10.1",
    crossPaths := false,
    publishMavenStyle := true,
    pomExtra :=
	<url>http://www.github.com/p2t2/figaro</url>
	<developers>
	  <developer>
	    <name>Avrom J. Pfeffer</name>
	    <email>apfeffer@cra.com</email>
	    <organization>Charles River Analytics, Inc.</organization>
	    <organizationUrl>http://www.cra.com</organizationUrl>
	  </developer>
	</developers>
	<licenses>
	  <license>
	    <name>Figaro License</name>
	    <url>https://github.com/p2t2/figaro/blob/master/LICENSE</url>
	  </license>
	</licenses>
	<scm>
	  <connection>scm:git:git@github.com:p2t2/figaro.git</connection>
	  <developerConnection>scm:git:git@github.com:p2t2/figaro.git</developerConnection>
	  <url>git@github.com:p2t2/figaro.git</url>
	</scm>)

  lazy val root = Project("root", file("."))
    .settings(publishLocal := {})
    .settings(publish := {})
    .dependsOn(figaro, examples)
    .aggregate(figaro, examples)

  lazy val figaro = Project("figaro", file("Figaro"))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation"
    ))
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-actors" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "asm" % "asm" % "3.3.1",
      "net.sf.jsci" % "jsci" % "1.2",
      "org.scalatest" %% "scalatest" % "2.0" % "test"
    ))
    .settings(parallelExecution in Test := false)
  
  lazy val examples = Project("figaro-examples", file("FigaroExamples"))
    .dependsOn(figaro)
}
