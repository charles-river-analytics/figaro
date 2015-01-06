Figaro Programming Language & Core Libraries
=
## What is Figaro?

Reasoning under uncertainty requires taking what you know and inferring what you don’t know, when what you know doesn’t tell you for sure what you don’t know. A well-established approach for reasoning under uncertainty is probabilistic reasoning. Typically, you create a probabilistic model over all the variables you’re interested in, observe the values of some variables, and query others. There is a huge variety of probabilistic models, and new ones are being developed constantly. Figaro is designed to help build and reason with the wide range of probabilistic models.
Developing a new probabilistic model normally requires developing a representation for the model and a reasoning algorithm that can draw useful conclusions from evidence, and in many cases also an algorithm to learn aspects of the model from data. These can be challenging tasks, making probabilistic reasoning require significant effort and expertise. Furthermore, most probabilistic reasoning tools are standalone and difficult to integrate into larger programs. 
Figaro is a probabilistic programming language that helps address both these issues. Figaro makes it possible to express probabilistic models using the power of programming languages, giving the modeler the expressive tools to create all sorts of models. Figaro comes with a number of built-in reasoning algorithms that can be applied automatically to new models. In addition, Figaro models are data structures in the Scala programming language, which is interoperable with Java, and can be constructed, manipulated, and used directly within any Scala or Java program.
Figaro is extremely expressive. It can represent a wide variety of models, including:

* directed and undirected models
* models in which conditions and constraints are expressed by arbitrary Scala functions
* models involving inter-related objects
* open universe models in which we don’t know what or how many objects exist
* models involving discrete and continuous elements
* models in which the elements are rich data structures such as  trees
* models with structured decisions
* models with unknown parameters

Figaro provides a rich library of constructs to build these models, and provides ways to extend this library to create your own model elements.

Figaro’s library of reasoning algorithms is also extensible. Current built-in algorithms include:

* Exact inference using variable elimination
* Belief propagation
* Particle belief propagation (experimental)
* Lazy factored inference for infinite models
* Importance sampling
* Metropolis-Hastings, with an expressive language to define proposal distributions
* Support computation
* Most probable explanation (MPE) using variable elimination or simulated annealing
* Probability of evidence using importance sampling
* Particle filtering
* Factored frontier
* Parameter learning using expectation maximization

Figaro provides both regular (the algorithm is run once) and anytime (the algorithm is run until stopped) versions of some of these algorithms. In addition to the built-in algorithms, Figaro provides a number of tools for creating your own reasoning algorithms.

Figaro is free and is released under an open-source license (see license file). The public code repository for Figaro can also be found at https://github.com/p2t2.

## Where can I get Figaro binary distributions?

The latest stable Figaro binary release is available for download from the [Charles River Analytics, Inc. Web site](http://www.cra.com/figaro).

Each binary release comes with Figaro, all required libraries, Scaladoc, examples, and source code.

## How can I use Figaro in my project?

If you wish to integrate Figaro's features into your own software project, Figaro is available on [Maven Central](http://search.maven.org). Shown below are a few examples of how you can add Figaro as a dependency to your existing project:

Simple Build Tool (SBT) Projects
```
libraryDependencies += "com.cra.figaro" %%  "figaro" % "3.0.0.0"
```

Apache Maven Projects
```
<dependency>
    <groupId>com.cra.figaro</groupId>
    <artifactId>figaro_2.11</artifactId>
    <version>3.0.0.0</version>
</dependency>
```

Apache Ivy Projects
```
<dependency org="com.cra.figaro" name="figaro_2.11" rev="3.0.0.0" />
```

## How do I run the Figaro Examples?

The easiest way to run the Figaro examples is to install Scala and use the latest Figaro binary release.

To get started, download Scala from [http://scala-lang.org/download/](http://scala-lang.org/download/). You will need Scala version 2.11.2 or later to run the latest Figaro release. Follow the Scala installation instructions at [http://scala-lang.org/download/install.html](http://scala-lang.org/download/install.html) and make sure you can run, compile, and execute the “Hello World” program provided in the documentation.

The next step is to obtain Figaro. The Figaro binary distribution is hosted at the Charles River Analytics, Inc. Web site at [https://www.cra.com/figaro](https://www.cra.com/figaro). The current version, as of January 2015, is 3.0.0.0 and is available for Scala 2.11. Make sure the Figaro version you use matches the Scala version. Each available download link is a compressed archive containing the Figaro jar (jar is the Java/Scala format for compiled byte code), examples, documentation, Scaladoc, and source code files. Click the appropriate link and then uncompress the downloaded archive to access the files. In each distribution, you will find a Figaro jar with a name ending with “fat” (such as “figaro_2.11-3.0.0.0-fat.jar”), indicating that this is a fat jar containing all the necessary libraries to run Figaro.

The final step is to open a command prompt and switch to the uncompressed Figaro download directory. Using the Scala command line program, run any Figaro example by setting the Scala classpath and invoking the desired class:
```
scala -cp figaro_2.11-3.0.0.0-fat.jar;figaroexamples_2.11-3.0.0.0.jar <example_class>
```

For example, to run the Burglary program:
```
scala -cp figaro_2.11-3.0.0.0-fat.jar;figaroexamples_2.11-3.0.0.0.jar com.cra.figaro.example.Burglary
```

## What Figaro Example classes are available?

The following examples are available in the Figaro Examples JAR file included in the binary release:
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

## How do I compile Figaro from source code?

Figaro is maintained as open source on GitHub. The GitHub project is Probabilistic Programming Tools and Techniques (P2T2), located at [https://github.com/p2t2](https://github.com/p2t2). P2T2 currently contains the Figaro sources, but we plan to update it with more tools. If you want to see the source code and build Figaro yourself, please visit our GitHub site.

To build Figaro from GitHub source, make a fork of the repository to your GitHub account, then use git’s clone feature to get the source code from your GitHub account to your machine.
```
git clone https://github.com/[your-github-username]/figaro.git
```

There are several branches available; checkout “master” for the latest stable release or the latest “DEV” branch for more cutting edge work and features (this is work in progress and therefore less stable). 

Figaro uses Simple Build Tool (SBT) to manage builds, located at [http://www.scala-sbt.org/](http://www.scala-sbt.org/). Download and install SBT, open a command prompt, and enter this SBT command set:
```
sbt clean compile package publishLocal assembly copy-deps
```

This will create Figaro for Scala 2.11; you will find the resulting artifacts in the “target” directory.

To run the Figaro unit tests, use this SBT command
```
sbt test
```

Note that some of the unit tests may not always pass because their results are non-deterministic.
