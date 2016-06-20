/*
 * StructuredAlgorithm.scala
 * Abstract class for algorithms that are structured
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   December 30, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm.OneTimeProbQuery
import com.cra.figaro.algorithm.Algorithm
import com.cra.figaro.language._
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Semiring
import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.algorithm.structured.ComponentCollection

abstract class StructuredProbQueryAlgorithm(val universe: Universe, val queryTargets: Element[_]*) extends Algorithm with OneTimeProbQuery {

  def run(): Unit

  val semiring: Semiring[Double]

  val targetFactors: Map[Element[_], Factor[Double]] = Map()

  val cc: ComponentCollection = new ComponentCollection

  val problem = new Problem(cc, queryTargets.toList)
  // We have to add all active elements to the problem since these elements, if they are every used, need to have components created at the top level problem
  universe.permanentElements.foreach(problem.add(_))
  val evidenceElems = universe.conditionedElements ::: universe.constrainedElements
  
  def initialComponents() = (problem.targets ++ evidenceElems).distinct.map(cc(_))

  protected def marginalizeToTarget(target: Element[_], jointFactor: Factor[Double]): Unit = {
    val targetVar = cc(target).variable
    val unnormalizedTargetFactor = jointFactor.marginalizeTo(targetVar)
    val z = unnormalizedTargetFactor.foldLeft(0.0, _ + _)
    val targetFactor = unnormalizedTargetFactor.mapTo((d: Double) => d / z)
    targetFactors += target -> targetFactor
  }

  /**
   * Computes the normalized distribution over a single target element.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    val targetVar = cc(target).variable
    val dist = factor.getIndices.filter(f => targetVar.range(f.head).isRegular).map(f => (factor.get(f), targetVar.range(f.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  /**
   * Computes the expectation of a given function for single target element.
   */
  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }

}


