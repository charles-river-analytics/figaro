import sbt._
import Keys._

object FigaroBuild extends Build {

  lazy val figaro = Project("figaro", file("Figaro"))
    .settings(version := "2.0.0.0")
    .settings(scalaVersion := "2.10.0")
    .settings(scalaSource in Compile <<= baseDirectory { _ / "src" })
    .settings(scalaSource in Test <<= baseDirectory { _ / "test" })
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-actors" % "2.10.0",
      "org.scala-lang" % "scala-reflect" % "2.10.0",
      "asm" % "asm" % "3.3.1",
      "asm" % "asm-tree" % "3.3.1",
      "asm" % "asm-analysis" % "3.3.1",
      "net.sf.jsci" % "jsci" % "1.2",
      "org.parboiled" % "parboiled-core" % "1.0.2",
      "org.parboiled" % "parboiled-java" % "1.0.2",
      "org.pegdown" % "pegdown" % "1.1.0",
      "org.scalatest" %% "scalatest" % "2.0" % "test"
    ))
  
  lazy val examples = Project("figaro-examples", file("FigaroExamples"))
    .dependsOn(figaro)
    .settings(scalaSource in Compile <<= baseDirectory { _ / "src" })

}
