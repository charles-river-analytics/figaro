package com.cra.figaro.experimental.structured.solver

import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Variable
import com.cra.figaro.algorithm.factored.beliefpropagation.FactorGraph
import com.cra.figaro.algorithm.factored.beliefpropagation.BasicFactorGraph
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.factored.factors.ElementVariable
import com.cra.figaro.algorithm.factored.beliefpropagation.VariableNode
import com.cra.figaro.algorithm.factored.beliefpropagation.Node
import com.cra.figaro.algorithm.factored.factors.LogSumProductSemiring

class BPSolver(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]])
extends com.cra.figaro.algorithm.factored.beliefpropagation.OneTimeProbabilisticBeliefPropagation {
  override val debug = false

  def generateGraph() = {
    factorGraph = new BasicFactorGraph(factors.map(makeLogarithmic(_)), semiring): FactorGraph[Double]
  }

  override def initialize() = {
    if (factorGraph == null) generateGraph()
    super.initialize
  }

  private def getNodeForVariable(variable: Variable[_]): Node = {
    val targetNode = factorGraph.getNodes.find { node =>
      node match {
        case vn: VariableNode => vn.variable == variable
        case _ => false
      }
    }
    targetNode.get
  }

  private def getFinalFactorForVariable(variable: Variable[_]): Factor[Double] = {
    beliefMap(getNodeForVariable(variable))
  }

  def go(): List[Factor[Double]] = {
    initialize()
    run()
    for { variable <- toPreserve.toList } yield {
      val factor = getFinalFactorForVariable(variable)
      unmakeLogarithmic(normalize(factor))
    }
  }

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = getBeliefsForElement(target).toStream

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }

  val iterations = 100

  val dependentUniverses = null

  val dependentAlgorithm = null

  val universe = null

  val semiring = LogSumProductSemiring()

  val targetElements = null
}
