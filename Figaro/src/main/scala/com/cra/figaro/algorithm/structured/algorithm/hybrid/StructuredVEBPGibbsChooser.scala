/*
 * StructuredVEBPGibbsChooser.scala
 * A hybrid algorithm that chooses between VE, BP, and Gibbs sampling for each component.
 *
 * Created By:      William kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 11, 2015
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.structured.algorithm.hybrid

import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.factored.gibbs.Gibbs
import com.cra.figaro.algorithm.factored.gibbs.BlockSampler

class StructuredVEBPGibbsChooser(universe: Universe, scoreThreshold: Double, determThreshold: Double, bpIters: Int, numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator, targets: Element[_]*)
  extends StructuredProbQueryAlgorithm(universe, targets: _*) with DecompositionProbQuery {

  def solvingStrategy() = new VEBPGibbsStrategy(problem, structuredRaising, scoreThreshold, determThreshold, bpIters, numSamples, burnIn, interval, blockToSampler)
}

object StructuredVEBPGibbsChooser {
  /**
   * Create a hybrid algorithm that chooses between variable elimination and Gibbs sampling on each subproblem.
   */
  def apply(scoreThreshold: Double, determThreshold: Double, bpIters: Int, numSamples: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE/Gibbs with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new StructuredVEBPGibbsChooser(targets(0).universe, scoreThreshold, determThreshold, bpIters, numSamples, 0, 1, BlockSampler.default, targets: _*)
  }

  /**
   * Create a hybrid algorithm that chooses between variable elimination and Gibbs sampling on each subproblem.
   */
  def apply(scoreThreshold: Double, determThreshold: Double, bpIters: Int, numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE/Gibbs with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new StructuredVEBPGibbsChooser(targets(0).universe, scoreThreshold, determThreshold, bpIters, numSamples, burnIn, interval, blockToSampler, targets: _*)
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean, threshold: Double, determThreshold: Double): Double = {
    val alg = this(threshold, determThreshold, 1000, 10000, 0, 1, BlockSampler.default, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T, threshold: Double = 0.0, determThreshold: Double = 0.5): Double =
    probability(target, (t: T) => t == value, threshold, determThreshold)
}
