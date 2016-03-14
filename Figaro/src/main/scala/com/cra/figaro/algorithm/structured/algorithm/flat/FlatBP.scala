/*
 * FlatBP.scala
 * A structured factored inference algorithm using belief propagation.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm.flat

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.strategy.decompose._
import com.cra.figaro.algorithm.factored.factors.factory._

class FlatBP(universe: Universe, iterations: Int, targets: Element[_]*) extends StructuredAlgorithm(universe, targets:_*) {

  val semiring = SumProductSemiring()
  
  def run() {    
    val strategy = DecompositionStrategy.recursiveFlattenStrategy(problem, new ConstantStrategy(beliefPropagation(iterations)), defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)
    val joint = problem.solution.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))  
    targets.foreach(t => marginalizeToTarget(t, joint))
  }

  
}

object FlatBP {
  /**
   * Create a structured belief propagation algorithm.
   * @param iterations the number of iterations to use for each subproblem
   * @param targets the query targets, which will all be part of the top level problem
   */
  def apply(iterations: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new FlatBP(targets(0).universe, iterations, targets:_*)
  }

  /**
   * Use BP to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean, iterations: Int): Double = {
    val alg = FlatBP(iterations, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use BP to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T, iterations: Int = 100): Double =
    probability(target, (t: T) => t == value, iterations)
}
