package com.cra.figaro.algorithm.factored.beliefpropagation

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.util
import scala.collection.mutable.{ Set, Map }

/**
 * Variable elimination algorithm to compute the most probable explanation.
 *
 * @param showTiming Produce timing information on steps of the algorithm
 */
abstract class MPEBeliefPropagation(override val universe: Universe)(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double) 
  extends MPEAlgorithm with ProbabilisticBeliefPropagation {

  override val semiring = MaxProductSemiring
  /**
   * Empty for MPE Algorithms
   */
  val targetElements = List[Element[_]]()

  val queryTargets = universe.activeElements

  val factorGraph = new BasicFactorGraph(getFactors(Seq()), semiring.product)

  def mostLikelyValue[T](target: Element[T]): T = {
    val beliefs = getBeliefsForElement(target)
    beliefs.maxBy(_._1)._2
  }
  
}

object MPEBeliefPropagation {
  /**
   * Create a most probable explanation computer using variable elimination
   * with the given target query variables in the current default
   * universe.
   */
  def apply(myIterations: Int)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with OneTimeProbabilisticBeliefPropagation with OneTimeMPE { val iterations = myIterations }

    def apply()(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with AnytimeProbabilisticBeliefPropagation with AnytimeMPE
    
  /**
   * Create a most probable explanation computer using variable elimination
   * with the given target query variables and using the given
   * dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], myIterations: Int)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      false,
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with OneTimeProbabilisticBeliefPropagation with OneTimeMPE { val iterations = myIterations }

    
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])])(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      false,
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with AnytimeProbabilisticBeliefPropagation with AnytimeMPE
      
  /**
   * Create a most probable explanation computer using variable elimination
   * with the given target query variables and using the given
   * dependent universes in the current default universe. Use the given dependent algorithm function to
   * determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, myIterations: Int)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      false,
      dependentUniverses,
      dependentAlgorithm)
      with OneTimeProbabilisticBeliefPropagation with OneTimeMPE { val iterations = myIterations }

  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)(implicit universe: Universe) =
    new MPEBeliefPropagation(universe)(
      false,
      dependentUniverses,
      dependentAlgorithm)
      with AnytimeProbabilisticBeliefPropagation with AnytimeMPE

}

