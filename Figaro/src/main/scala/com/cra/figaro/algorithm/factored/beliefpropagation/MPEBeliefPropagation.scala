/*
 * MPEBeliefPropagation.scala  
 * An MPE algorithm using BP
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 15, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.beliefpropagation

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.util
import scala.collection.mutable.{ Set, Map }

/**
 * BP algorithm to compute the most probable explanation.
 */
abstract class MPEBeliefPropagation(override val universe: Universe)(
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends MPEAlgorithm with ProbabilisticBeliefPropagation {

  override val semiring = MaxProductSemiring()
  /*
   * Empty for MPE Algorithms
   */
  val targetElements = List[Element[_]]()

  override def initialize() = {
    val (neededElements, _) = getNeededElements(universe.activeElements, Int.MaxValue)
    factorGraph = new BasicFactorGraph(getFactors(neededElements, targetElements), logSpaceSemiring()): FactorGraph[Double]
    super.initialize
  }

  /*
   * Convert factors to use MaxProduct
   */
  override def getFactors(allElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[Double]] = {
    val factors = super.getFactors(allElements, targetElements, upper)
    // Not needed since BP now converts factors to log space of the defined semiring
    //factors.map (_.mapTo(x => x, logSpaceSemiring()))
    factors
  }

  def mostLikelyValue[T](target: Element[T]): T = {
    val beliefs = getBeliefsForElement(target)
    beliefs.maxBy(_._1)._2
  }

}

object MPEBeliefPropagation {
  /**
   * Create a most probable explanation computer using One time BP
   * in the current default universe.
   */
  def apply(myIterations: Int)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeMPE { val iterations = myIterations }

  /**
   * Create a most probable explanation computer using Anytime BP
   * in the current default universe.
   */
  def apply()(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with AnytimeProbabilisticBeliefPropagation with AnytimeMPE

  /**
   * Create a most probable explanation computer using One time BP using the given
   * dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], myIterations: Int)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeMPE { val iterations = myIterations }

  /**
   * Create a most probable explanation computer using Anytime BP using the given
   * dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])])(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with AnytimeProbabilisticBeliefPropagation with AnytimeMPE

  /**
   * Create a most probable explanation computer using One time BP
   * using the given dependent universes in the current default universe.
   * Use the given dependent algorithm function to
   * determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, myIterations: Int)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      dependentUniverses,
      dependentAlgorithm) with OneTimeProbabilisticBeliefPropagation with OneTimeMPE { val iterations = myIterations }

  /**
   * Create a most probable explanation computer using Anytime BP
   * using the given dependent universes in the current default universe.
   * Use the given dependent algorithm function to
   * determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      dependentUniverses,
      dependentAlgorithm) with AnytimeProbabilisticBeliefPropagation with AnytimeMPE

}

