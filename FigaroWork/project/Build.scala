/*
 * Build.scala 
 * The Figaro Work project SBT build program.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com)
 * Creation Date:   Aug 6, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

import sbt._
import Keys._

object FigaroWorkBuild extends Build {

  override val settings = super.settings ++ Seq(
    scalaVersion := "2.11.4"
  )

  lazy val figaroWork = Project("FigaroWork", file("."))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation",
	"-language:postfixOps"
    ))
    .settings(libraryDependencies ++= Seq(
      "com.cra.figaro" %% "figaro" % "latest.release"
    ))
}
