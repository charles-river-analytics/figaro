/*
 * LazyStructuredVE.scala
 * A structured variable elimination algorithm.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured.algorithm.`lazy`

import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.strategy.refine.RefiningStrategy
import com.cra.figaro.algorithm.structured.strategy.refine.PartialBottomUpStrategy

abstract class LazyStructuredVE(universe: Universe, targets: Element[_]*)
  extends LazyStructuredProbQueryAlgorithm(universe, targets: _*) {

  /**
   * Depth to which to expand the model at the current iteration.
   */
  def depth(): Int

  /**
   * Initial elements to pass to the bottom-up strategy for decomposition. Defaults to a list containing all problem
   * targets and all evidence elements in the universe.
   */
  val initialElements: List[Element[_]] = {
    (problemTargets ::: universe.conditionedElements ::: universe.constrainedElements).distinct
  }

  override def initialize(): Unit = {
    super.initialize()
    collection.useSingleChainFactor = true
  }

  override def refiningStrategy(): RefiningStrategy =
    new PartialBottomUpStrategy(depth(), problem, initialElements.map(collection(_)))

  override def solvingStrategy(): SolvingStrategy =
    new ConstantStrategy(problem, structuredRaising, marginalVariableElimination)
}

// One time lazy VE uses a fixed depth
class OneTimeLazyVE(override val depth: Int, universe: Universe, targets: Element[_]*)
  extends LazyStructuredVE(universe, targets: _*) with OneTimeLazyStructuredProbQuery

// Anytime lazy VE increases depth by the given increment at each iteration
class AnytimeLazyVE(depthIncrement: Int, universe: Universe, targets: Element[_]*)
  extends LazyStructuredVE(universe, targets: _*) with AnytimeLazyStructuredProbQuery {

  // Current depth of expansion
  private var currentDepth = 0

  override def depth(): Int = {
    // Increment depth each time we create a refining strategy
    currentDepth += depthIncrement
    currentDepth
  }
}

object LazyStructuredVE {
  /**
   * Creates a one time lazy structured variable elimination algorithm that expands to the given depth.
   * @param depth Nonnegative depth of expansion.
   * @param targets Query targets.
   * @return A one time lazy structured VE algorithm.
   */
  def apply(depth: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new OneTimeLazyVE(depth, targets(0).universe, targets: _*)
  }

  /**
   * Creates an anytime lazy structured variable elimination algorithm that expands the depth by 1 at each iteration.
   * @param targets Query targets.
   * @return An anytime lazy structured VE algorithm.
   */
  def apply(targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new AnytimeLazyVE(1, targets(0).universe, targets: _*)
  }

  /**
   * Creates an anytime lazy structured variable elimination algorithm that expands the depth by the given increment at
   * each iteration.
   * @param depthIncrement Depth by which to increase the expansion at each iteration. Must be positive.
   * @param targets Query targets.
   * @return An anytime lazy structured VE algorithm.
   */
  def anytime(depthIncrement: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new AnytimeLazyVE(depthIncrement, targets(0).universe, targets: _*)
  }
}