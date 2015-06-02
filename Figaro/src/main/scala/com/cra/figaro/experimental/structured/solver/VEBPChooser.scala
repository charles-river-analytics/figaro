/*
 * VEBPChooser.scala
 * A hybrid solver that chooses between variable elimination and belief propagation.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.structured.solver

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.experimental.structured.Problem
import com.cra.figaro.util.HashMultiSet
import com.cra.figaro.util.MultiSet

/*
 * The score threshold is the threshold to determine whether VE or BP will be used.
 * This score represents the maximum increase in cost of the factors through using VE, compared to the initial factors.
 */
private[figaro] class VEBPChooser(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]],
                  val scoreThreshold: Double, val iterations: Int)
extends VariableElimination[Double] {
  def go(): List[Factor[Double]] = {
    val (score, order) = VariableElimination.eliminationOrder(factors, toPreserve)
//    print("Score = " + score + " - ")
    if (score > scoreThreshold) {
//      println("Choosing BP")
      val bp = new BPSolver(problem, toEliminate, toPreserve, factors, iterations)
      result = bp.go()
    } else {
//      println("Choosing VE")
      // Since we've already computed the order, we don't call doElimination but only do the steps after computing the order
      val factorsAfterElimination = eliminateInOrder(order, HashMultiSet(factors: _*), initialFactorMap(factors))
      finish(factorsAfterElimination, order)
    }
    result
  }

  val semiring: Semiring[Double] = SumProductSemiring()

  private var result: List[Factor[Double]] = _

  def finish(factorsAfterElimination: MultiSet[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit = {
    result = factorsAfterElimination.toList
  }

  val dependentAlgorithm: (com.cra.figaro.language.Universe, List[com.cra.figaro.language.NamedEvidence[_]]) => () => Double = null

  val dependentUniverses: List[(com.cra.figaro.language.Universe, List[com.cra.figaro.language.NamedEvidence[_]])] = null

  def getFactors(neededElements: List[com.cra.figaro.language.Element[_]],
                 targetElements: List[com.cra.figaro.language.Element[_]],
                upperBounds: Boolean): List[com.cra.figaro.algorithm.factored.factors.Factor[Double]] = null

   val showTiming: Boolean = false

   val targetElements: List[com.cra.figaro.language.Element[_]] = null

   val universe: com.cra.figaro.language.Universe = null

}
