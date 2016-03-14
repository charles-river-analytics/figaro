/*
 * FlatGibbs.scala
 * A flat Gibbs sampling algorithm.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 8, 2015
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
import com.cra.figaro.algorithm.factored.gibbs._
import com.cra.figaro.algorithm.factored.gibbs.BlockSampler

class FlatGibbs(universe: Universe, numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator, targets: Element[_]*)
  extends StructuredAlgorithm(universe, targets: _*) {

  val semiring = SumProductSemiring()

  def run() {   
    val strategy = DecompositionStrategy.recursiveFlattenStrategy(problem, new ConstantStrategy(gibbs(numSamples, burnIn, interval, blockToSampler)), defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)
    val joint = problem.solution.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))
    targets.foreach(t => marginalizeToTarget(t, joint))
  }

}

object FlatGibbs {
  /**
   * Create a flat Gibbs algorithm.
   */
  def apply(numSamples: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run Gibbs with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new FlatGibbs(targets(0).universe, numSamples, 0, 1, BlockSampler.default, targets: _*)
  }

  /**
   * Create a flat Gibbs algorithm.
   */
  def apply(numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run Gibbs with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new FlatGibbs(targets(0).universe, numSamples, burnIn, interval, blockToSampler, targets: _*)
  }

  /**
   * Use Gibbs to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    val alg = this(10000, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use Gibbs to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T): Double =
    probability(target, (t: T) => t == value)
}
