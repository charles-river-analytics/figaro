/*
 * StructuredMarginalMAPAlgorithm.scala
 * SFI algorithms that compute marginal MAP queries.
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

import com.cra.figaro.algorithm.AlgorithmException
import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.algorithm._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.{Bounds, ComponentCollection}
import com.cra.figaro.language._

/**
 * A structured marginal MAP algorithm.
 * @param universe Universe on which to perform inference.
 * @param mapElements Elements for which to compute MAP queries. Elements not in this sequence are summed over.
 */
abstract class StructuredMarginalMAPAlgorithm(universe: Universe, collection: ComponentCollection, val mapElements: Element[_]*)
  extends StructuredAlgorithm(universe, collection) with MarginalMAPAlgorithm {

  def this(universe: Universe, mapElements: Element[_]*) {
    this(universe, new ComponentCollection, mapElements:_*)
  }

  override def problemTargets = mapElements.toList

  // Solutions contain MPE values of individual variables, and are precisely the problem's recording factors.
  protected var targetFactors: Map[Variable[_], Factor[_]] = Map()

  override def processSolutions(solutions: Map[Bounds, Solution]): Unit = {
    if(solutions.size > 1) {
      throw new IllegalArgumentException("this model requires lower and upper bounds; " +
        "use a lazy algorithm instead, or a ranging strategy that avoids *")
    }
    val (_, recordingFactors) = solutions.head._2
    targetFactors = recordingFactors
  }

  /**
   * Returns the most likely value for the target element.
   * Throws an IllegalArgumentException if the range of the target contains star.
   */
  def computeMostLikelyValue[T](target: Element[T]): T = {
    val targetVar = collection(target).variable
    if (targetVar.valueSet.hasStar) {
      throw new IllegalArgumentException("target range contains *; " +
        "use a lazy algorithm instead, or a ranging strategy that avoids *")
    }
    val factor = targetFactors(targetVar).asInstanceOf[Factor[T]]
    if (factor.size != 1) throw new AlgorithmException//("Final factor for most likely value has more than one entry")
    factor.get(List())
  }
}

trait OneTimeStructuredMarginalMAP extends StructuredMarginalMAPAlgorithm with OneTimeStructured with OneTimeMarginalMAP

trait AnytimeStructuredMarginalMAP extends StructuredMarginalMAPAlgorithm with AnytimeStructured with AnytimeMarginalMAP

trait DecompositionMarginalMAP extends OneTimeStructuredMarginalMAP with DecompositionStructuredAlgorithm
