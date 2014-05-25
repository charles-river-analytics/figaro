organization := "com.cra.figaro"

name := "figaro"

version := "2.1.1.0-SNAPSHOT"

scalaVersion:= "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.1")

libraryDependencies ++= Seq(
   "org.scala-lang"     %  "scala-actors"   % scalaVersion.value,
   "org.scala-lang"     %  "scala-reflect"  % scalaVersion.value,
   "asm"                % "asm"             % "3.3.1",
   "net.sf.jsci"        % "jsci"            % "1.2",
   "org.scalatest"      %% "scalatest"      % "2.1.7"     % "test"
)

