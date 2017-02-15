/*
 * StructuredMarginalMAPAlgorithm.scala
 * Abstract class for structured marginal MAP algorithms.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 3, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm.{Algorithm, AlgorithmException}
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.algorithm.structured.{ComponentCollection, Problem}
import com.cra.figaro.language._

/**
 * A structured marginal MAP algorithm.
 * @param universe Universe on which to perform inference.
 * @param mapElements Elements for which to compute MAP queries. Elements not in this list are summed over.
 */
abstract class StructuredMarginalMAPAlgorithm(val universe: Universe, val mapElements: List[Element[_]])
  extends Algorithm with OneTimeMarginalMAP {

  def run(): Unit

  val cc: ComponentCollection = new ComponentCollection

  // Targets are our MAP elements, since the first step is to eliminate the other elements
  val problem = new Problem(cc, mapElements)
  
  // We have to add all active elements to the problem since these elements, if they are every used, need to have components created at the top level problem
  universe.permanentElements.foreach(problem.add(_))
  val evidenceElems = universe.conditionedElements ::: universe.constrainedElements

  def initialComponents() = (problem.targets ++ evidenceElems).distinct.map(cc(_))

  def computeMostLikelyValue[T](target: Element[T]): T = {
    val targetVar = cc(target).variable
    val factor = problem.recordingFactors(targetVar).asInstanceOf[Factor[T]]
    if (factor.size != 1) throw new AlgorithmException//("Final factor for most likely value has more than one entry")
    factor.get(List())
  }
 

}


