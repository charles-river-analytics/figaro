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

For more information see the [Figaro Guide](https://www.cra.com/figaro/figaroguide.html).
