import sbt._
import Keys._

object FigaroWorkBuild extends Build {

  override val settings = super.settings ++ Seq(
    scalaVersion := "2.11.2"
  )

  lazy val figaroWork = Project("FigaroWork", file("."))
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation"
    ))
    .settings(libraryDependencies ++= Seq(
      "com.cra.figaro" %% "figaro" % "latest.release"
    ))
}
