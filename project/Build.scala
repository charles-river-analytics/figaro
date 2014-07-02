import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import sbt.Package.ManifestAttributes

object FigaroBuild extends Build {

  override val settings = super.settings ++ Seq(
    organization := "com.cra.figaro",
    description := "Figaro: a language for probablistic programming",
    version := "2.2.2.0",
    scalaVersion := "2.10.4",
    //scalaVersion := "2.11.1",
    crossScalaVersions := Seq("2.10.4", "2.11.1"),
    crossPaths := true,
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
	</scm>,
    packageOptions := Seq(ManifestAttributes(
      ("Bundle-RequiredExecutionEnvironment", "JavaSE-1.6")
    ))
  )

  lazy val root = Project("root", file("."))
    .settings(publishLocal := {})
    .settings(publish := {})
    .dependsOn(figaro, examples)
    .aggregate(figaro, examples)

  lazy val figaro = Project("Figaro", file("Figaro"))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation"
    ))
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-actors" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "asm" % "asm" % "3.3.1",
      "org.apache.commons" % "commons-math3" % "3.3",
      "net.sf.jsci" % "jsci" % "1.2",
      "com.typesafe.akka" %% "akka-actor" % "2.3.3",
      "org.scalatest" %% "scalatest" % "2.1.7" % "test"
    ))
    .settings(parallelExecution in Test := false)
    .settings(assemblySettings: _*)
    .settings(test in assembly := {})
    .settings(jarName in assembly := "figaro_" + scalaVersion.value + "-" + version.value + "-fat.jar")
    .settings(assemblyOption in assembly ~= { _.copy(includeScala = false) })
      
  lazy val examples = Project("FigaroExamples", file("FigaroExamples"))
    .dependsOn(figaro)
}
