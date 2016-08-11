/*
 * MarginalMAPBeliefPropagation.scala
 * A marginal MAP belief propagation algorithm. Based on the algorithm by Liu and Ihler (2013).
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 10, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.factored.beliefpropagation._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language._

abstract class MarginalMAPBeliefPropagation(override val universe: Universe, targets: Element[_]*)(
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends MarginalMAPAlgorithm
  with ProbabilisticBeliefPropagation {

  val targetElements = targets.toList
  
  val mapElements = targetElements

  /*
   * Variables corresponding to MAP elements. This is set in the initialize() method.
   */
  protected var maxVariables: Set[Variable[_]] = _

  /**
   * Value used to compute arg max messages. This could be thought of as an inverse
   * "temperature", but here it is a large fixed value.
   */
  val argMaxFactor = 1e5
  
  override protected def getNewMessageFactorToVar(fn: FactorNode, vn: VariableNode) = {
    val vnFactor = factorGraph.getLastMessage(vn, fn)
    
    if(maxVariables.contains(vn.variable)) {
      val total = beliefMap(fn).combination(vnFactor, LogSumProductSemiring().divide)
      
      // Use sum-product to sum over sum variables
      val sumOverSumVars = total.marginalizeTo(fn.variables.intersect(maxVariables).toSeq:_*)
      // Use max-product to sum over max variables except vn.variable
      val sumOverMaxVars = sumOverSumVars.marginalizeToWithSum(LogMaxProductSemiring().sum, vn.variable)
      sumOverMaxVars
    }
    else {
      // Use sum-product to sum over sum variables, note that we don't divide by the last message here
      val beliefOverMaxVars = beliefMap(fn).marginalizeTo(fn.variables.intersect(maxVariables).toSeq:_*)
      val maxBelief = beliefOverMaxVars.foldLeft(LogSumProductSemiring().zero, _ max _)
      // Filter the indices that maximize the  belief over the max variables in this factor
      // This approximation of the "indicator function" is used for two reasons:
      // 1) To prevent division by zero when we divide the belief map by the last message
      // 2) To give nonzero weight to values close to the arg max that differ due to floating point errors
      val argMaxIndicator = beliefOverMaxVars.mapTo(d => (d - maxBelief) * argMaxFactor)
      
      // Now the total includes the indicator function
      val total = beliefMap(fn).combination(vnFactor, LogSumProductSemiring().divide).product(argMaxIndicator)
      
      // Use sum-product to sum over all variables except vn.variable
      val sumOverAllVars = total.marginalizeTo(vn.variable)
      sumOverAllVars
    }
  }

  /*
   * We use sum product by default and switch to max product only when needed.
   * Notably, we only use the sum operation through getNewMessageFactorToVar.
   */
  val semiring = SumProductSemiring()

  override def initialize() = {
    val (neededElements, _) = getNeededElements(universe.activeElements, Int.MaxValue)
    factorGraph = new BasicFactorGraph(getFactors(neededElements, targetElements), logSpaceSemiring())
    maxVariables = factorGraph.getNodes.collect {
      case VariableNode(ev: ElementVariable[_]) if mapElements.contains(ev.element) => ev
    }.toSet
    super.initialize()
  }

  def computeMostLikelyValue[T](target: Element[T]): T = {
    val beliefs = getBeliefsForElement(target)
    beliefs.maxBy(_._1)._2
  }
}

object MarginalMAPBeliefPropagation {
  /**
   * Creates a One Time marginal MAP belief propagation computer in the current default universe.
   * @param myIterations Iterations of mixed-product BP to run.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      List(), (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with OneTimeProbabilisticBeliefPropagation with OneTimeMarginalMAP { val iterations = myIterations }

  /**
   * Creates an Anytime marginal MAP belief propagation computer in the current default universe.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      List(), (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with AnytimeProbabilisticBeliefPropagation with AnytimeMarginalMAP

  /**
   * Creates a One Time marginal MAP belief propagation computer in the current default universe.
   * @param dependentUniverses Dependent universes for this algorithm.
   * @param myIterations Iterations of mixed-product BP to run.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      dependentUniverses, (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with OneTimeProbabilisticBeliefPropagation with OneTimeMarginalMAP { val iterations = myIterations }

  /**
   * Creates an Anytime marginal MAP belief propagation computer in the current default universe.
   * @param dependentUniverses Dependent universes for this algorithm.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      dependentUniverses, (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with AnytimeProbabilisticBeliefPropagation with AnytimeMarginalMAP

  /**
   * Creates a One Time marginal MAP belief propagation computer in the current default universe.
   * @param dependentUniverses Dependent universes for this algorithm.
   * @param dependentAlgorithm Used to determine algorithm for computing probability of evidence in dependent universes.
   * @param myIterations Iterations of mixed-product BP to run.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      dependentUniverses, dependentAlgorithm)
      with OneTimeProbabilisticBeliefPropagation with OneTimeMarginalMAP { val iterations = myIterations }

  /**
   * Creates an Anytime marginal MAP belief propagation computer in the current default universe.
   * @param dependentUniverses Dependent universes for this algorithm.
   * @param dependentAlgorithm Used to determine algorithm for computing probability of evidence in dependent universes.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      dependentUniverses, dependentAlgorithm)
      with AnytimeProbabilisticBeliefPropagation with AnytimeMarginalMAP
  
  /**
   * Use belief propagation to compute the most likely value of the given element.
   * Runs 10 iterations of mixed-product BP.
   * @param target Element for which to compute MAP value.
   * @param mapElements Additional elements to MAP. Elements not in this list are summed over.
   */
  def mostLikelyValue[T](target: Element[T], mapElements: Element[_]*): T = {
    val alg = MarginalMAPBeliefPropagation(10, (target +: mapElements).distinct:_*)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}