Figaro Programming Language & Core Libraries
============================================
Figaro is a probabilistic programming language that supports development of very rich probabilistic models and  provides reasoning algorithms that can be applied to models to draw useful conclusions from evidence. Both model representation and reasoning algorithm development can be challenging tasks.  

Figaro makes it possible to express probabilistic models using the power of programming languages, giving the modeler the expressive tools to create a wide variety of models. Figaro comes with a number of built-in reasoning algorithms that can be applied automatically to new models. In addition, Figaro models are data structures in the Scala programming language, which is interoperable with Java, and can be constructed, manipulated, and used directly within any Scala or Java program.

The current, stable Figaro binary release (as well as previous versions) can be found at https://www.cra.com/figaro


Development
-----------
Figaro can be built using Ant or SBT.

### SBT ###
To build with SBT, clone this repository and `cd` into it, then run `sbt`.
In SBT you can
 * run tests with `test`
 * generate the documentation with `doc`
 * package a distributable jar with `package`
