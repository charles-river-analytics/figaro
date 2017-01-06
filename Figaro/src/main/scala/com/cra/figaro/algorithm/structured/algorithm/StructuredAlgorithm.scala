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
   * The list of bounds for which this algorithm should compute solutions. This defaults to just lower bounds, because
   * standard implementations don't have laziness (or disallow star values). Lazy algorithms will generally override
   * this to compute both lower and upper bounds.
   * @return All bounds for which this algorithm should compute solutions.
   */
  def allBounds(): List[Bounds] = List(Lower)

  /**
   * Extract the solution in a way that allows fast queries to the algorithm.
   * @param bounds Bounds for which to record the solution.
   */
  def recordSolution(bounds: Bounds): Unit

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
    for(bounds <- allBounds()) {
      solvingStrategy().execute(bounds)
      recordSolution(bounds)
    }
  }
}

trait AnytimeStructured extends StructuredAlgorithm with Anytime

trait OneTimeStructured extends StructuredAlgorithm with OneTime {
  // One time structured algorithms run refinement and solving just once each.
  override def run(): Unit = runStep()
}
