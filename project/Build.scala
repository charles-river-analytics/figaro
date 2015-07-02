/*
 * Build.scala 
 * The Figaro project SBT build program.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com)
 * Creation Date:   Feb 17, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import sbt.Package.ManifestAttributes
import scoverage.ScoverageSbtPlugin._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys

object FigaroBuild extends Build {

  override val settings = super.settings ++ Seq(
    organization := "com.cra.figaro",
    description := "Figaro: a language for probablistic programming",
    version := "3.2.1.0",
    scalaVersion := "2.11.6",
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
	</scm>
  )

  lazy val scalaMajorMinor = "2.11"

  // Read exisiting Figaro MANIFEST.MF from file
  lazy val figaroManifest = Using.fileInputStream(file("Figaro/META-INF/MANIFEST.MF")) { 
    in => new java.util.jar.Manifest(in)
  }

  // Read exisiting FigaroExamples MANIFEST.MF from file
  lazy val examplesManifest = Using.fileInputStream(file("FigaroExamples/META-INF/MANIFEST.MF")) {
    in => new java.util.jar.Manifest(in)
  }

  lazy val root = Project("root", file("."))
    .settings(publishLocal := {})
    .settings(publish := {})
    .dependsOn(figaro, examples)
    .aggregate(figaro, examples)

  lazy val figaro = Project("Figaro", file("Figaro"))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation",
	"-language:postfixOps"
    ))
    .settings(packageOptions := Seq(Package.JarManifest(figaroManifest)))
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "asm" % "asm" % "3.3.1",
      "org.apache.commons" % "commons-math3" % "3.3",
      "net.sf.jsci" % "jsci" % "1.2",
      "com.typesafe.akka" %% "akka-actor" % "2.3.8",
      "org.scalanlp" %% "breeze" % "0.10",
      "io.argonaut" %% "argonaut" % "6.0.4",
      "org.prefuse" % "prefuse" % "beta-20071021",
      "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
      "com.storm-enroute" %% "scalameter" % "0.6" % "provided",
      "org.scalatest" %% "scalatest" % "2.2.4" % "provided, test"
    ))
    // Copy all managed dependencies to \lib_managed directory
    .settings(retrieveManaged := true)
    // Enable forking
    .settings(fork := true)
    // Increase max memory for JVM for both testing and runtime
    .settings(javaOptions in (Test,run) += "-Xmx8G")
    // test settings
    .settings(parallelExecution in Test := false)
    .settings(testOptions in Test += Tests.Argument("-oD"))
    .configs(detTest)
    .settings(inConfig(detTest)(Defaults.testTasks): _*)
    .settings(testOptions in detTest := Seq(Tests.Argument("-l", "com.cra.figaro.test.nonDeterministic")))
    .configs(nonDetTest)
    .settings(inConfig(nonDetTest)(Defaults.testTasks): _*)
    .settings(testOptions in nonDetTest := Seq(Tests.Argument("-n", "com.cra.figaro.test.nonDeterministic")))
    // sbt-assembly settings
    .settings(assemblySettings: _*)
    .settings(test in assembly := {})
    .settings(jarName in assembly := "figaro_" + scalaMajorMinor + "-" + version.value + "-fat.jar")
    .settings(assemblyOption in assembly ~= { _.copy(includeScala = false) })
    .settings(excludedJars in assembly := {
	val cp = (fullClasspath in assembly).value
	cp filter {_.data.getName == "arpack_combined_all-0.1-javadoc.jar"}
    })
    // ScalaMeter settings
    .settings(testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))
    .settings(logBuffered := false)
    // SBTEclipse settings
    .settings(EclipseKeys.eclipseOutput := Some("target/scala-2.11/classes"))
      
  lazy val examples = Project("FigaroExamples", file("FigaroExamples"))
    .dependsOn(figaro)
    .settings(packageOptions := Seq(Package.JarManifest(examplesManifest)))
    // SBTEclipse settings
    .settings(EclipseKeys.eclipseOutput := Some("target/scala-2.11/classes"))

  lazy val detTest = config("det") extend(Test)
  lazy val nonDetTest = config("nonDet") extend(Test)
}
