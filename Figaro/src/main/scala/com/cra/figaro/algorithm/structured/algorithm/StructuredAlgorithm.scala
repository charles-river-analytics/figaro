/*
 * StructuredAlgorithm.scala
 * Algorithms for structured factored inference.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Dec 21, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy.refine.RefiningStrategy
import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy
import com.cra.figaro.language._

/**
 * Structured algorithms that perform inference on a problem by a sequence of refining and solving steps. The algorithm
 * runs in a single universe.
 */
abstract class StructuredAlgorithm extends Algorithm {
  /**
   * Universe to which elements in the corresponding problem belong.
   */
  def universe: Universe

  /**
   * List of targets that should not be eliminated when solving the problem.
   * @return Targets for the problem.
   */
  def problemTargets: List[Element[_]]

  /**
   * Strategy to use for refinement at a single iteration. This may return a new strategy for each iteration.
   * @return A refining strategy to be used for a single iteration.
   */
  def refiningStrategy(): RefiningStrategy

  /**
   * Strategy to use for solving at a single iteration. This may return a new strategy for each iteration.
   * @return A solving strategy to be used for a single iteration.
   */
  def solvingStrategy(): SolvingStrategy

  /**
   * All bounds for which this algorithm needs to compute solutions. This is determined by looking for components that
   * have * in their range, and have constraint factors associated with them. If such a component exists, we need both
   * lower and upper bounds. Otherwise, just one of the bounds suffices because they are equivalent; it defaults to
   * lower in this case.
   * @return All bounds for which this algorithm should compute solutions.
   */
  def neededBounds(): Set[Bounds] = {
    if(problem.components.exists(comp => comp.range.hasStar && comp.constraintFactors().nonEmpty)) Set(Lower, Upper)
    else Set(Lower)
  }

  /**
   * Subclasses must define the type of solutions to be stored for querying. In general, this should allow fast queries,
   * but in general the representation depends on the particular class of algorithm.
   */
  type ProcessedSolution

  /**
   * The processed solutions associated with the most recent solving run. After solving, this map contains a key for
   * each of the bounds that were determined necessary by `neededBounds()`, and a value corresponding to the solution
   * produced thereafter by `extractSolution()`. For thread safety purposes, this is explicitly an immutable map, as it
   * allows us to guarantee that all entries in the map correspond to solutions from a single iteration of `runStep()`.
   */
  protected var processedSolutions: Map[Bounds, ProcessedSolution] = Map()

  /**
   * Extract the solution in a way that allows fast queries to the algorithm.
   * @return A processed solution that can be stored in `processedSolutions`, computed based on the current solution to
   * the problem.
   */
  def extractSolution(): ProcessedSolution

  /**
   * Collection containing all components that the problem or its subproblems use.
   */
  val collection = new ComponentCollection()

  /**
   * Inference problem to be solved.
   */
  val problem = new Problem(collection, problemTargets)

  /**
   * Initialize the problem by adding all permanent elements to it. This is to ensure that all top-level elements are
   * correctly added to the top-level problem.
   */
  override def initialize(): Unit = {
    super.initialize()
    universe.permanentElements.foreach(problem.add(_))
  }

  /**
   * Run a single iteration of refinement/solving, then record the solutions.
   */
  def runStep(): Unit = {
    refiningStrategy().execute()
    processedSolutions = neededBounds().map(bounds => {
      solvingStrategy().execute(bounds)
      bounds -> extractSolution()
    }).toMap
  }
}

trait AnytimeStructured extends StructuredAlgorithm with Anytime

trait OneTimeStructured extends StructuredAlgorithm with OneTime {
  // One time structured algorithms run refinement and solving just once each.
  override def run(): Unit = runStep()
}
