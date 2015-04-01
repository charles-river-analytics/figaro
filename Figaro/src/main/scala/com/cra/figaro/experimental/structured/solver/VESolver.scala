package com.cra.figaro.experimental.structured.solver

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.experimental.structured.Problem

class VESolver(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]])
extends com.cra.figaro.algorithm.factored.VariableElimination[Double] {
  val semiring: Semiring[Double] = SumProductSemiring()

  private var result: List[Factor[Double]] = _

  def finish(factorsAfterElimination: scala.collection.mutable.Set[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit = {
    result = factorsAfterElimination.toList
  }

  def go(): List[Factor[Double]] = {
    doElimination(factors, toPreserve.toList)
    result
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
