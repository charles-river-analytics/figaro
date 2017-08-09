/*
 * StructuredMPEAlgorithm.scala
 * SFI algorithms that compute MPE queries.
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   December 30, 2015
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.Bounds
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.language._

abstract class StructuredMPEAlgorithm(val universe: Universe) extends StructuredAlgorithm with MPEAlgorithm {
  // This is an empty list because MPE works by eliminating all variables.
  override def problemTargets = List()

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
  def mostLikelyValue[T](target: Element[T]): T = {
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

trait OneTimeStructuredMPE extends StructuredMPEAlgorithm with OneTimeStructured with OneTimeMPE

trait AnytimeStructuredMPE extends StructuredMPEAlgorithm with AnytimeStructured with AnytimeMPE
