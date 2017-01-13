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

package com.cra.figaro.algorithm.structured.algorithm.hybrid

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.strategy.decompose._
import com.cra.figaro.algorithm.factored.factors.factory._

class StructuredVEBPChooser(universe: Universe, scoreThreshold: Double, BPIterations: Int, targets: Element[_]*)
  extends StructuredProbQueryAlgorithm(universe, targets: _*) {

  val semiring = SumProductSemiring()

  def run() {
    val strategy = DecompositionStrategy.recursiveStructuredStrategy(problem, new VEBPStrategy(scoreThreshold, BPIterations), defaultRangeSizer, Lower, false)
    strategy.execute(initialComponents)
    val joint = problem.solution.foldLeft(Factory.unit(semiring))(_.product(_))
    targets.foreach(t => marginalizeToTarget(t, joint))
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
    new StructuredVEBPChooser(targets(0).universe, scoreThreshold, BPIterations, targets: _*)
  }

  /**
   * Use the hybrid algorithm to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean, threshold: Double, iterations: Int): Double = {
    val alg = StructuredVEBPChooser(threshold, iterations, target)
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
