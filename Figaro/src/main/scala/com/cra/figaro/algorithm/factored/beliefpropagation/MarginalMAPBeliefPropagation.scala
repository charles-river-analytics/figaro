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

package com.cra.figaro.algorithm.factored.beliefpropagation

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.algorithm.{MarginalMAPAlgorithm, OneTimeMarginalMAP}
import com.cra.figaro.language._

abstract class MarginalMAPBeliefPropagation(override val universe: Universe, targets: Element[_]*)(
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends MarginalMAPAlgorithm
  with ProbabilisticBeliefPropagation {

  val targetElements = targets.toList
  
  val mapElements = targetElements
  
  var maxVariables: Set[Variable[_]] = _
  
  override protected def getNewMessageFactorToVar(fn: FactorNode, vn: VariableNode) = {
    val vnFactor = factorGraph.getLastMessage(vn, fn)
    
    if(maxVariables.contains(vn.variable)) {
      val total = beliefMap(fn).combination(vnFactor, LogSumProductSemiring().divide)
      
      // Use sum-product to sum over sum variables
      val sumOverSumVars = total.marginalizeTo(LogSumProductSemiring(), fn.variables.filter(maxVariables.contains).toSeq:_*)
      // Use max-product to sum over max variables except vn.variable
      val sumOverMaxVars = sumOverSumVars.mapTo(d => d, LogMaxProductSemiring())
                                         .marginalizeTo(LogMaxProductSemiring(), vn.variable)
                                         .mapTo(d => d, LogSumProductSemiring())
      sumOverMaxVars
    }
    else {
      // Note that we don't divide by the last message here
      val beliefOverMaxVars = beliefMap(fn).marginalizeTo(LogSumProductSemiring(), fn.variables.filter(maxVariables.contains).toSeq:_*)
      val maxBelief = beliefOverMaxVars.contents.maxBy(_._2)._2
      // Filter the indices that maximize the  belief over the max variables in this factor
      val argMaxIndicator = beliefOverMaxVars.mapTo(d => if(d == maxBelief) 1.0 else 0.0, LogSumProductSemiring())
      
      // Now the total includes the indicator function
      val total = beliefMap(fn).combination(vnFactor, LogSumProductSemiring().divide).product(argMaxIndicator)
      
      // Use sum-product to sum over all variables except vn.variable
      val sumOverAllVars = total.marginalizeTo(LogSumProductSemiring(), vn.variable)
      sumOverAllVars
    }
  }

  /*
   * For now, we use sum product by default and switch to max product when needed.
   * Notably, we only use the sum operation through getNewMessageFactorToVar.
   * TODO: look into better ways to do this.
   */
  val semiring = SumProductSemiring()

  override def initialize() = {
    def filterMaxVariables(node: Node): Option[Variable[_]] = node match {
      case VariableNode(ev: ElementVariable[_]) => if(mapElements.contains(ev.element)) Some(ev) else None
      case _ => None
    }
    
    val (neededElements, _) = getNeededElements(universe.activeElements, Int.MaxValue)
    factorGraph = new BasicFactorGraph(getFactors(neededElements, targetElements), logSpaceSemiring())
    maxVariables = factorGraph.getNodes.flatMap(filterMaxVariables).toSet
    super.initialize
  }

  def computeMostLikelyValue[T](target: Element[T]): T = {
    val beliefs = getBeliefsForElement(target)
    beliefs.maxBy(_._1)._2
  }
}

object MarginalMAPBeliefPropagation {
  /**
   * Creates a One Time marginal MAP belief propagation computer in the current default universe.
   * @param myIterations Iterations of BP to run.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeMarginalMAP { val iterations = myIterations }

  /**
   * Creates a One Time marginal MAP belief propagation computer in the current default universe.
   * @param dependendUniverses Dependent universes for this algorithm.
   * @param myIterations Iterations of BP to run.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeMarginalMAP { val iterations = myIterations }

  /**
   * Creates a One Time marginal MAP belief propagation computer in the current default universe.
   * @param dependendUniverses Dependent universes for this algorithm.
   * @param dependendAlgorithm Used to determine algorithm for computing probability of evidence in dependent universes.
   * @param myIterations Iterations of BP to run.
   * @param targets MAP elements, which can be queried. Elements not supplied here are summed over.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new MarginalMAPBeliefPropagation(universe, targets: _*)(
      dependentUniverses,
      dependentAlgorithm) with OneTimeProbabilisticBeliefPropagation with OneTimeMarginalMAP { val iterations = myIterations }

  // TODO Anytime versions
  
  /**
   * Use variable elimination to compute the most likely value of the given element.
   * @param target Element for which to compute MAP value.
   * @param mapElements Additional elements to MAP. Elements not in this list are summed over.
   */
  def mostLikelyValue[T](target: Element[T], mapElements: List[Element[_]]): T = {
    val alg = MarginalMAPBeliefPropagation(10, (target :: mapElements).distinct:_*)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
  
    /**
   * Use marginal MAP belief propagation to compute the most likely value of the given element.
   * @param target Element for which to compute MAP value. All other elements in the universe are summed over.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    val alg = MarginalMAPBeliefPropagation(10, target)
    alg.start()
    val result = alg.mostLikelyValue(target)
    alg.kill()
    result
  }
}