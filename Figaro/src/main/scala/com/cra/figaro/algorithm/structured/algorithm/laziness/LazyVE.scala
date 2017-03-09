/*
 * StructuredVE.scala
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

package com.cra.figaro.algorithm.structured.algorithm.laziness

import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.Lower
import com.cra.figaro.algorithm.structured.strategy.refine.RefiningStrategy
import com.cra.figaro.algorithm.structured.strategy.refine.PartialBottomUpStrategy
import com.cra.figaro.algorithm.structured.strategy.refine._

abstract class LazyVE(universe: Universe, targets: Element[_]*) extends StructuredProbQueryAlgorithm(universe, targets: _*)
    with LazyStructuredProbQuery {

  var depth: Int = 1

  /**
   * Initial elements to pass to the bottom-up strategy for decomposition. Defaults to a list containing all problem
   * targets and all evidence elements in the universe.
   */
  def initialElements: List[Element[_]] = {
    (problemTargets ::: universe.conditionedElements ::: universe.constrainedElements).distinct
  }

  override def initialize(): Unit = {
    super.initialize()
    collection.useSingleChainFactor = true
  }

  def solvingStrategy() = new ConstantStrategy(problem, structuredRaising, marginalVariableElimination)

  def computeProbabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    // TODO is this really correct even if the target range doesn't contain star?
    val targetVar = collection(target).variable
    val factor = processedSolutions(Lower)(target)
    val index = targetVar.range.indexWhere { x => x.isRegular && x.value == value }
    if (index < 0) (0.0, 1.0)
    else {
      val prob = factor.get(List(index))
      val starIndex = targetVar.range.indexWhere { x => !x.isRegular }
      if (starIndex >= 0) {
        (prob, prob + factor.get(List(starIndex)))
      } else {
        (prob, prob)
      }
    }
  }

  def refiningStrategy(): RefiningStrategy = {
    new PartialBottomUpStrategy(depth, problem, defaultRangeSizer, initialElements.map(collection(_)))
  }

}

class OneTimeLazyVE(initialDepth: Int, universe: Universe, targets: Element[_]*) extends LazyVE(universe, targets: _*) with OneTimeLazyStructuredProbQuery {
  depth = initialDepth
}

class AnytimeLazyVE(universe: Universe, targets: Element[_]*) extends LazyVE(universe, targets: _*) with AnytimeLazyStructuredProbQuery {
  depth = 1
  
  override def runStep(): Unit = {
    depth = depth + 1
    super.runStep()
  }
}

object LazyVE {
  /** Create a structured variable elimination algorithm with the given query targets. */
  def apply(depth: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new OneTimeLazyVE(depth, targets(0).universe, targets: _*)
  }

  def apply(targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new AnytimeLazyVE(targets(0).universe, targets: _*)
  }

  /*
  /**
   * Use VE to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    val alg = StructuredVE(target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use VE to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T): Double =
    probability(target, (t: T) => t == value)
    * 
    */
}