Figaro Programming Language & Core Libraries
=
Figaro is a probabilistic programming language that supports development of very rich probabilistic models and  provides reasoning algorithms that can be applied to models to draw useful conclusions from evidence. Both model representation and reasoning algorithm development can be challenging tasks.  

Figaro makes it possible to express probabilistic models using the power of programming languages, giving the modeler the expressive tools to create a wide variety of models. Figaro comes with a number of built-in reasoning algorithms that can be applied automatically to new models. In addition, Figaro models are data structures in the Scala programming language, which is interoperable with Java, and can be constructed, manipulated, and used directly within any Scala or Java program.

## What algorithms are supported in Figaro?

Current built-in algorithms include:
```
Exact inference using variable elimination
Importance sampling
Metropolis-Hastings, with an expressive language to define proposal distributions
Support computation
Most probable explanation (MPE) using variable elimination or simulated annealing
Probability of evidence using importance sampling
Particle Filtering
Parameter learning using expectation maximization
```

Figaro provides both regular (the algorithm is run once) and anytime (the algorithm is run until stopped) versions of some of these algorithms. In addition to the built-in algorithms, Figaro provides a number of tools for creating your own reasoning algorithms.

## Where can I get Figaro binary distributions?

Figaro “fat JAR” binary distributions are available for download from the [Charles River Analytics, Inc. Web site](http://www.cra.com/figaro).

Each binary bundle comes with all required Figaro libraries, Scaladoc, examples, and complete source code.

## What Scala versions are supported by Figaro?

Figaro supports Scala 2.11.  Scala 2.10 users should use Figaro v2.2.2.0.

## How can I use Figaro in my project?

Figaro and its dependencies are available on [Maven Central](http://search.maven.org). Shown below are a few examples of how you can integrate Figaro into your existing software project:

Simple Build Tool (SBT) Projects
```
libraryDependencies += "com.cra.figaro" %%  "figaro" % "2.5.0.0"
```

Apache Maven Projects
```
<dependency>
    <groupId>com.cra.figaro</groupId>
    <artifactId>figaro_2.11</artifactId>
    <version>2.5.0.0</version>
</dependency>
```

Apache Ivy Projects
```
<dependency org="com.cra.figaro" name="figaro_2.11" rev="2.5.0.0" />
```

## How do I run the Figaro Examples?

1) Download and install Scala 2.11

2) Download the latest Figaro binary bundle for Scala 2.11 and uncompress the archive

3) Open a command prompt

4) Switch to the uncompressed Figaro binary bundle directory, and use the Scala command line:
```
scala -cp figaro_2.11-2.5.0.0-fat.jar;figaroexamples_2.11-2.5.0.0.jar <example_class>
```

For instance:
```
scala -cp figaro_2.11-2.5.0.0-fat.jar;figaroexamples_2.11-2.5.0.0.jar com.cra.figaro.example.Burglary
```

## What Figaro Example classes are available?

The following examples are available in the Figaro Examples JAR file included in the binary bundle:
```
com.cra.figaro.example.AnnealingSmokers
com.cra.figaro.example.Burglary
com.cra.figaro.example.CarAndEngine
com.cra.figaro.example.CoinExample
com.cra.figaro.example.DiceExample
com.cra.figaro.example.Entrepreneur
com.cra.figaro.example.Firms
com.cra.figaro.example.Hierarchy
com.cra.figaro.example.LazyList
com.cra.figaro.example.MultiDecision
com.cra.figaro.example.MultiValuedReferenceUncertainty
com.cra.figaro.example.MutableMovie
com.cra.figaro.example.OpenUniverse
com.cra.figaro.example.OpenUniverseLearning
com.cra.figaro.example.SimpleLearning
com.cra.figaro.example.SimpleMovie
com.cra.figaro.example.Smokers
com.cra.figaro.example.Sources
com.cra.figaro.example.dosage.DosageDecision
com.cra.figaro.example.graph.GraphDecision
```
