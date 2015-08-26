/*
 * StructuredVEGibbsChooser.scala
 * A hybrid algorithm that chooses between variable elimination and Gibbs sampling for each component.
 *
 * Created By:      William kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 11, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.structured.algorithm


import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.experimental.factored._
import com.cra.figaro.experimental.structured.ComponentCollection
import com.cra.figaro.experimental.structured.Lower
import com.cra.figaro.experimental.structured.Problem
import com.cra.figaro.experimental.structured.factory.Factory
import com.cra.figaro.experimental.structured.strategy.decompose.RecursiveStrategy
import com.cra.figaro.experimental.structured.strategy.decompose.defaultRangeSizer
import com.cra.figaro.experimental.structured.strategy.solve.VEGibbsStrategy
import com.cra.figaro.language._

class StructuredVEGibbsChooser(val universe: Universe, scoreThreshold: Double, numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator, targets: Element[_]*)
  extends Algorithm with OneTimeProbQuery {
  val queryTargets = targets

  var targetFactors: Map[Element[_], Factor[Double]] = _

  var cc: ComponentCollection = _

  def run() {
    cc = new ComponentCollection
    targetFactors = Map()
    val problem = new Problem(cc, targets.toList)
    val evidenceElems = universe.conditionedElements ::: universe.constrainedElements
    evidenceElems.foreach(elem => if (!cc.contains(elem)) problem.add(elem))
    (new RecursiveStrategy(problem, new VEGibbsStrategy(scoreThreshold, numSamples, burnIn, interval, blockToSampler), defaultRangeSizer, Lower, false)).execute
    val joint = problem.solution.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))

    def marginalizeToTarget(target: Element[_]): Unit = {
      val targetVar = cc(target).variable
      val unnormalizedTargetFactor = joint.marginalizeTo(SumProductSemiring(), targetVar)
      val z = unnormalizedTargetFactor.foldLeft(0.0, _ + _)
      val targetFactor = unnormalizedTargetFactor.mapTo((d: Double) => d / z)
      targetFactors += target -> targetFactor
    }

    targets.foreach(marginalizeToTarget(_))
  }


  /**
   * Computes the normalized distribution over a single target element.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    val targetVar = cc(target).variable
    val dist = factor.getIndices.filter(f => targetVar.range(f.head).isRegular).map(f => (factor.get(f), targetVar.range(f.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

 /**
   * Computes the expectation of a given function for single target element.
   */
  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }
}

object StructuredVEGibbsChooser {
  /**
   * Create a hybrid algorithm that chooses between variable elimination and Gibbs sampling on each subproblem.
   */
  def apply(scoreThreshold: Double, numSamples: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE/Gibbs with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new StructuredVEGibbsChooser(targets(0).universe, scoreThreshold, numSamples, 0, 1, BlockSampler.default, targets:_*)
  }

  /**
   * Create a hybrid algorithm that chooses between variable elimination and Gibbs sampling on each subproblem.
   */
  def apply(scoreThreshold: Double, numSamples: Int, burnIn: Int, interval: Int, blockToSampler: Gibbs.BlockSamplerCreator, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE/Gibbs with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new StructuredVEGibbsChooser(targets(0).universe, scoreThreshold, numSamples, burnIn, interval, blockToSampler, targets:_*)
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean, threshold: Double): Double = {
    val alg = this(threshold, 10000, 0, 1, BlockSampler.default, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T, threshold: Double = 0.0): Double =
    probability(target, (t: T) => t == value, threshold)
}
