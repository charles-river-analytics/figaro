/*
 * StructuredVEBPChooser.scala
 * A hybrid algorithm that chooses between variable elimination and belief propagation for each component.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.structured.algorithm

import com.cra.figaro.algorithm.OneTimeProbQuery
import com.cra.figaro.algorithm.Algorithm
import com.cra.figaro.language.Element
import com.cra.figaro.language.Universe
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.experimental.structured.ComponentCollection
import com.cra.figaro.experimental.structured.Problem
import com.cra.figaro.experimental.structured.strategy.decompose._
import com.cra.figaro.experimental.structured.solver.chooseVEOrBP
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import com.cra.figaro.experimental.structured.factory.Factory
import com.cra.figaro.experimental.structured.Lower
import com.cra.figaro.experimental.structured.strategy.solve.VEBPStrategy

class StructuredVEBPChooser(val universe: Universe, scoreThreshold: Double, BPIterations: Int, targets: Element[_]*)
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
    (new RecursiveStrategy(problem, new VEBPStrategy(scoreThreshold, BPIterations), defaultRangeSizer, Lower, false)).execute        
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

object StructuredVEBPChooser {
  /**
   * Create a hybrid algorithm that chooses between variable elimination and belief propagation on each subproblem.
   * @param scoreThreshold The minimum value of the increase in score caused by eliminating a variable that causes the hybrid algorithm to
   * choose BP for a subproblem.
   * @param bpIterations The number of iterations to use when BP is chosen for a subproblem.
   * @param targets The query targets
   */
  def apply(scoreThreshold: Double, BPIterations: Int, targets: Element[_]*) = {
    if (targets.isEmpty) throw new IllegalArgumentException("Cannot run VE with no targets")
    val universes = targets.map(_.universe).toSet
    if (universes.size > 1) throw new IllegalArgumentException("Cannot have targets in different universes")
    new StructuredVEBPChooser(targets(0).universe, scoreThreshold, BPIterations, targets:_*)
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean, threshold: Double, iterations: Int): Double = {
    val alg = StructuredVEBPChooser(0.0, iterations, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T, threshold: Double = 0.0, iterations: Int = 100): Double =
    probability(target, (t: T) => t == value, threshold, iterations)
}
