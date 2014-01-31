package com.cra.figaro.algorithm.factored.beliefpropagation

import com.cra.figaro.algorithm.MPEAlgorithm
import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.language.Element
import com.cra.figaro.language.Universe

/*
/**
 * Most probable explanation version of belief propagation.
 */
class MPEBeliefPropagation(override val universe: Universe)
  extends MPEAlgorithm(universe)
  with OneTimeMPE
  with BeliefPropagation {

  /**
   * Usual multiplication.
   */
  def product(x: Double, y: Double) = x * y

  /**
   * Addition is max.
   */
  def sum(x: Double, y: Double) = x max y

  def mostLikelyValue[T](target: Element[T]) = {
    // TODO
    computeDistribution(target).toList.max(Ordering.by((_: (Double, T))._1))._2
  }
}

object MaxProductBeliefPropagation {
  /**
   * Creates a most probable explanation computer using belief propagation in the
   * current default universe.
   */
  def apply(implicit universe: Universe = Universe.universe) =
    new MPEBeliefPropagation(universe)
}
*/