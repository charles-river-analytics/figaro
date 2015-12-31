/*
 * BPSolver.scala
 * A belief propagation solver.
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
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors.LogSumProductSemiring
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.algorithm.factored.beliefpropagation.FactorNode
import com.cra.figaro.algorithm.factored.factors.factory.Factory._
import com.cra.figaro.algorithm.factored.beliefpropagation._
import com.cra.figaro.algorithm.structured._

private[figaro] class BPSolver(problem: Problem, toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]], val iterations: Int)
extends com.cra.figaro.algorithm.factored.beliefpropagation.OneTimeProbabilisticBeliefPropagation {
  // We need to create a joint probability distribution over the interface to this nested subproblem.
  // To achieve this, we create a new variable representing the tuple of the attributes to preserve.
  // We create a factor to represent the tuple creation.
  // We then run BP as usual.
  // At the end, we sum the tuple variable out of this factor to obtain the solution.

  val (tupleVar, tupleFactor): (Variable[_], Factor[Double]) = makeTupleVarAndFactor(problem.collection, None, toPreserve.toList:_*)

  def generateGraph() = {
    val allFactors = tupleFactor :: factors
    factorGraph = new BasicFactorGraph(allFactors.map(makeLogarithmic(_)), semiring): FactorGraph[Double]
  }

  override def initialize() = {
    if (factorGraph == null) generateGraph()
    super.initialize
  }

  def go(): List[Factor[Double]] = {
    initialize()
    run()
    val targetVars = toPreserve.toList ::: List(tupleVar)
    val tupleBelief = belief(FactorNode(toPreserve + tupleVar))
    val targetBelief = tupleBelief.sumOver(tupleVar)
    List(unmakeLogarithmic(normalize(targetBelief)))
  }

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = getBeliefsForElement(target).toStream

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }

  val dependentUniverses = null

  val dependentAlgorithm = null

  val universe = null

  val semiring = LogSumProductSemiring()

  val targetElements = null
}
