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

import com.cra.figaro.algorithm.Algorithm
import com.cra.figaro.language._
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.factored.factors.Semiring
import com.cra.figaro.algorithm.structured.Problem
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.AlgorithmException

abstract class StructuredMPEAlgorithm(val universe: Universe) extends Algorithm with OneTimeMPE {

  def run(): Unit

  val semiring: Semiring[Double]

  //val targetFactors: Map[Element[_], Factor[Double]] = Map()

  val cc: ComponentCollection = new ComponentCollection

  val problem = new Problem(cc, List())
  // We have to add all active elements to the problem since these elements, if they are every used, need to have components created at the top level problem
  universe.permanentElements.foreach(problem.add(_))
  val evidenceElems = universe.conditionedElements ::: universe.constrainedElements

  def initialComponents() = (universe.permanentElements ++ evidenceElems).distinct.map(cc(_))

  /**
   * Returns the most likely value for the target element.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    val targetVar = cc(target).variable
    val factor = problem.recordingFactors(targetVar).asInstanceOf[Factor[T]]
    if (factor.size != 1) throw new AlgorithmException//("Final factor for most likely value has more than one entry")
    factor.get(List())
  }
 

}


