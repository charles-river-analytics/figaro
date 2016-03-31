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

package com.cra.figaro.algorithm.structured.algorithm.structured

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve.ConstantStrategy
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.strategy.decompose._
import com.cra.figaro.algorithm.factored.factors.factory._


class StructuredVE(universe: Universe, targets: Element[_]*) extends StructuredProbQueryAlgorithm(universe, targets: _*) {

  val semiring = SumProductSemiring()

  def run() {    
    val strategy = DecompositionStrategy.recursiveStructuredStrategy(problem, new ConstantStrategy(marginalVariableElimination), defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)
    val joint = problem.solution.foldLeft(Factory.unit(semiring))(_.product(_))
    targets.foreach(t => marginalizeToTarget(t, joint))
  }
}

object StructuredVE {
  /** Create a structured variable elimination algorithm with the given query targets. */
  def apply(targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new StructuredVE(targets(0).universe, targets: _*)
  }

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
}
