//
// build.sbt
// Figaro SBT build script
//
// Created By:      Josh Serrin (jserrin@cra.com), Mike Reposa (mreposa@cra.com)
// Creation Date:   Jan 17, 2014
//
// Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
// See http://www.cra.com or email figaro@cra.com for information.
//
// See http://www.github.com/p2t2/figaro for a copy of the software license.
//

//
// Additional Updates from our community
// 
// Martin Mauch		Dec 18, 2013
// Dragisa Krsmanovic	May 24, 2014
// Paul Philips		May 23, 2017
//

import sbt._
import Keys._
import sbt.Package.ManifestAttributes
import com.typesafe.sbteclipse.core.EclipsePlugin._
import sbtassembly.Plugin._
import AssemblyKeys._
import scoverage.ScoverageSbtPlugin._

  name := "figaro-root"

  lazy val figaroSettings = Seq(
    organization := "com.cra.figaro",
    description := "Figaro: a language for probablistic programming",
    version := "5.1.0.0",
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq(scalaVersion.value, "2.11.8"),
    crossPaths := true,
    publishMavenStyle := true,
    retrieveManaged := true,
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

  // lazy val scalaMajorMinor = "2.12"

  // Read exisiting Figaro MANIFEST.MF from file
  lazy val figaroManifest = Using.fileInputStream(file("Figaro/META-INF/MANIFEST.MF")) { 
    in => new java.util.jar.Manifest(in)
  }

  // Read exisiting FigaroExamples MANIFEST.MF from file
  lazy val examplesManifest = Using.fileInputStream(file("FigaroExamples/META-INF/MANIFEST.MF")) {
    in => new java.util.jar.Manifest(in)
  }

  lazy val root = Project("root", file("."))
    .settings(figaroSettings)
    .settings(publishLocal := {})
    .settings(publish := {})
    .dependsOn(figaro, examples)
    .aggregate(figaro, examples)

  lazy val figaro = Project("Figaro", file("Figaro"))
    .settings(figaroSettings)
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation",
	"-language:postfixOps"
    ))
    .settings(packageOptions := Seq(Package.JarManifest(figaroManifest)))
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "net.sf.jsci" % "jsci" % "1.2",
      "com.typesafe.akka" %% "akka-actor" % "2.5.23",
      "org.scalanlp" %% "breeze" % "0.13.2",
      "io.argonaut" %% "argonaut" % "6.2.3",
      "com.storm-enroute" %% "scalameter" % "0.9" % "provided",
      "org.scalatest" %% "scalatest" % "3.0.7" % "provided, test"
    ))

    // Enable forking
    .settings(fork := true)
    // Increase max memory for JVM
    .settings(javaOptions += "-Xmx6G")
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
    // .settings(jarName in assembly := "figaro_" + scalaMajorMinor + "-" + version.value + "-fat.jar")
    .settings(jarName in assembly := "figaro_" + scalaBinaryVersion.value + "-" + version.value + "-fat.jar")
    .settings(assemblyOption in assembly ~= { _.copy(includeScala = false) })
    .settings(excludedJars in assembly := {
	val cp = (fullClasspath in assembly).value
	cp filter {_.data.getName == "arpack_combined_all-0.1-javadoc.jar"}
    })
    // ScalaMeter settings
    .settings(testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))
    .settings(logBuffered := false)
      
  lazy val examples = Project("FigaroExamples", file("FigaroExamples"))
    .dependsOn(figaro)
    .settings(figaroSettings)
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation",
	"-language:postfixOps"
    ))
    .settings(packageOptions := Seq(Package.JarManifest(examplesManifest)))

  lazy val detTest = config("det") extend(Test)
  lazy val nonDetTest = config("nonDet") extend(Test)
