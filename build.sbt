name := "figaro"

organization := "com.cra"

version := "2.0.0"

lazy val root = project.in(file(".")).aggregate(core, examples)

lazy val core = project.in(file("Figaro"))

lazy val examples = project.in(file("FigaroExamples")).dependsOn(core)
