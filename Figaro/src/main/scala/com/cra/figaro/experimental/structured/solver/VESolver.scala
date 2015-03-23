package com.cra.figaro.experimental.structured.solver

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import scala.collection.mutable.Set

class VESolver(toEliminate: List[Variable[_]], factors: List[Factor[Double]])
extends com.cra.figaro.algorithm.factored.VariableElimination[Double] {
  val semiring: com.cra.figaro.algorithm.factored.factors.Semiring[Double] = SumProductSemiring

  var globals: List[Variable[_]] = _

  var result: List[Factor[Double]] = _

  def finish(factorsAfterElimination: Set[Factor[Double]], eliminationOrder: List[Variable[_]]): Unit = {
    result = factorsAfterElimination.toList
  }

  def go() {
    val toPreserve = (Set[Variable[_]]() /: factors)(_ ++ _.variables) -- toEliminate.toSet
    globals = toPreserve.toList
    doElimination(factors, globals)
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
