name := "figaro-core"

libraryDependencies ++= Seq(
    "asm" % "asm-all" % "3.3.1",
    "org.parboiled" % "parboiled-core" % "1.0.2",
    "org.parboiled" % "parboiled-java" % "1.0.2",
    "org.pegdown" % "pegdown" % "1.1.0",
    "net.sf.jsci" % "jsci" % "1.2",
    "org.scalatest" %% "scalatest" % "2.0" % "test"
)

libraryDependencies <++= scalaVersion(v =>
  Seq(
    "org.scala-lang" % "scala-actors" % v,
    "org.scala-lang" % "scala-reflect" % v
    )
)

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

javaSource in Compile := baseDirectory.value / "src"

javaSource in Test := baseDirectory.value / "test"
