lazy val figaro = project.in( file("Figaro") )

lazy val figaroExamples = project.in( file("FigaroExamples") )
                                 .dependsOn(figaro)
