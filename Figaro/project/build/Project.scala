import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {

  override def mainScalaSourcePath = "src"

  override def compileOptions = super.compileOptions ++ Seq(Unchecked, Deprecation)

  val external = new ExternalIvyConfiguration(info.projectPath.asFile, 
    "ivysettings.xml".asFile, 
    Some(info.launcher.globalLock), 
    log)

  val scalatest = "org.scalatest" % "scalatest" % "1.2"

}