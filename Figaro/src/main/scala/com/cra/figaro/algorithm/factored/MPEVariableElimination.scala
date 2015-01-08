/*
 * MPEVariableElimination.scala
 * Variable elimination algorithm for computing most probable explanation.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.util
import scala.collection.mutable.{ Set, Map }
import com.cra.figaro.algorithm.lazyfactored._

/**
 * Variable elimination algorithm to compute the most probable explanation.
 * 
 * @param showTiming Produce timing information on steps of the algorithm
 */
class MPEVariableElimination(override val universe: Universe)(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double) extends OneTimeMPE with ProbabilisticVariableElimination {

  override val comparator = Some((x: Double, y: Double) => x < y)
  override val semiring = MaxProductSemiring
  
  /*
   * We are trying to find a configuration of all the elements, so we must make them all starter elements for expansion.
   */
  override val starterElements = universe.activeElements
  
  /**
   * Empty for MPE Algorithms.
   */
  val targetElements = List[Element[_]]()

  private val maximizers: Map[Variable[_], Any] = Map()

  private def getMaximizer[T](variable: Variable[T]): T = maximizers(variable).asInstanceOf[variable.Value]

  def mostLikelyValue[T](target: Element[T]): T = getMaximizer(Variable(target))

  private def backtrackOne[T](factor: Factor[_], variable: Variable[T]): Unit = {
    val indices =
      for { variable <- factor.variables } yield util.indices(variable.range, Regular(getMaximizer(variable))).head
    maximizers += variable -> factor.get(indices)
  }

  def finish(factorsAfterElimination: Set[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit =
    for { (variable, factor) <- eliminationOrder.reverse.zip(recordingFactors) } { backtrackOne(factor, variable) }
}

object MPEVariableElimination {
  /**
   * Create a most probable explanation computer using variable elimination
   * with the given target query variables in the current default
   * universe.
   */
  def apply()(implicit universe: Universe) =
    new MPEVariableElimination(universe)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a most probable explanation computer using variable elimination
   * with the given target query variables and using the given
   * dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])])(implicit universe: Universe) =
    new MPEVariableElimination(universe)(
      false,
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a most probable explanation computer using variable elimination
   * with the given target query variables and using the given
   * dependent universes in the current default universe. Use the given dependent algorithm function to
   * determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)(implicit universe: Universe) =
    new MPEVariableElimination(universe)(
      false,
      dependentUniverses,
      dependentAlgorithm)

}
