To run Figaro programs that you create:

1) Install SBT v0.13 for your chosen operating system
2) Copy your Figaro program files to FigaroWork\src\main\scala
3) Open a command line prompt
4) Navigate to your local FigaroWork directory
5) At the command prompt, type
	a) sbt "runMain <class_with_main> <parameters>"

Note: don't forget the quotes around the runMain command!

To test if your local environment is properly configured, run

	sbt "runMain Test"

You should see the following output

	[info] Running Test
	1.0


Here is a link to the complete SBT Tutorial:

	http://www.scala-sbt.org/0.13/tutorial/index.html
