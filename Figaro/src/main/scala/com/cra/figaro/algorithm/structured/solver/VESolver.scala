/*
 * VESolver.scala
 * A variable elimination solver.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.solver

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.util.MultiSet
import com.cra.figaro.algorithm.factored.factors.Semiring
import com.cra.figaro.util
import com.cra.figaro.algorithm.lazyfactored._

class VESolver(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]], val semiring: Semiring[Double])
  extends com.cra.figaro.algorithm.factored.VariableElimination[Double] {

  debug = false
  
  override val comparator = semiring match {
    case sum: SumProductSemiring => None
    case max: MaxProductSemiring => Some((x: Double, y: Double) => x < y)
  }

  def go(): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
    // Convert factors to MaxProduct for MPE
    val convertedFactors = semiring match {
      case sum: SumProductSemiring => factors
      case max: MaxProductSemiring => factors.map(_.mapTo(x => x, semiring))
    }
    doElimination(convertedFactors, toPreserve.toList)
    (resultFactors, recordingFactorsMap)
  }

  private var resultFactors: List[Factor[Double]] = _
  // A map from each variable to a factor that maps values of the toPreserve variables to maximal values of the variable
  // Note that when the toPreserve is empty, this represents the maximal value of each variable
  private var recordingFactorsMap: Map[Variable[_], Factor[_]] = Map()
  private def getRecordingFactor[T](variable: Variable[T]): Factor[T] = recordingFactorsMap(variable).asInstanceOf[Factor[variable.Value]]

  def finish(factorsAfterElimination: MultiSet[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit = {
    semiring match {
      case sum: SumProductSemiring => finishSum(factorsAfterElimination, eliminationOrder)
      case max: MaxProductSemiring => finishMax(factorsAfterElimination, eliminationOrder)
    }
  }

  /* Finish function for marginal VE */
  def finishSum(factorsAfterElimination: MultiSet[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit = {
    resultFactors = factorsAfterElimination.toList
  }

  /* Finish function for MPE VE */
  def finishMax(factorsAfterElimination: MultiSet[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit = {
    resultFactors = factorsAfterElimination.toList
    /* If empty, we need to know the max values for all variables in this set of factors
     *  Otherwise, we assume that the eliminated varaibles are internal and therefore are not queryable
     *  When we have non-chain decompositions, this may not hold anymore
     */
    if (factorsAfterElimination.forall(f => f.size == 1 && f.numVars == 0)) {
      for { (variable, factor) <- eliminationOrder.reverse.zip(recordingFactors) } { backtrackOne(factor, variable) }
    }
  }

  private def backtrackOne[T](factor: Factor[_], variable: Variable[T]): Unit = {
    val indices =
      for { variable <- factor.variables } yield util.indices(variable.range, Regular(getRecordingFactor(variable).contents.head._2)).head
    recordingFactorsMap += variable -> {
      val bf = factor.asInstanceOf[Factor[variable.Value]].createFactor(List(), List())
      bf.set(List(), factor.asInstanceOf[Factor[variable.Value]].get(indices))
      bf
    }
  }

  /* Functions not needed for SFI */
  val dependentAlgorithm: (com.cra.figaro.language.Universe, List[com.cra.figaro.language.NamedEvidence[_]]) => () => Double = null

  val dependentUniverses: List[(com.cra.figaro.language.Universe, List[com.cra.figaro.language.NamedEvidence[_]])] = null

  def getFactors(neededElements: List[com.cra.figaro.language.Element[_]],
    targetElements: List[com.cra.figaro.language.Element[_]],
    upperBounds: Boolean): List[com.cra.figaro.algorithm.factored.factors.Factor[Double]] = null

  val showTiming: Boolean = false

  val targetElements: List[com.cra.figaro.language.Element[_]] = null

  val universe: com.cra.figaro.language.Universe = null
}
