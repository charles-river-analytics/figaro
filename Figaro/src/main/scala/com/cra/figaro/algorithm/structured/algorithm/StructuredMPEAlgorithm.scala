/*
 * StructuredMPEAlgorithm.scala
 * SFI algorithms that compute MPE queries.
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

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured.{Bounds, Lower}
import com.cra.figaro.language._

abstract class StructuredMPEAlgorithm(val universe: Universe) extends StructuredAlgorithm with MPEAlgorithm {
  // This is an empty list because MPE works by eliminating all variables.
  override def problemTargets = List()

  // Solutions containing MPE values of individual variables for lower and upper bounds, respectively.
  private var lowerRecordingFactors: Map[Variable[_], Factor[_]] = Map()
  private var upperRecordingFactors: Map[Variable[_], Factor[_]] = Map()

  override def recordSolution(bounds: Bounds): Unit = {
    if(bounds == Lower) {
      lowerRecordingFactors = problem.recordingFactors
    }
    else {
      upperRecordingFactors = problem.recordingFactors
    }
  }

  /**
   * Returns the most likely value for the target element.
   * Throws an IllegalArgumentException if the range of the target contains star.
   */
  def mostLikelyValue[T](target: Element[T]): T = {
    val targetVar = collection(target).variable
    if(targetVar.valueSet.hasStar) throw new IllegalArgumentException("target range contains *")
    val factor = lowerRecordingFactors(targetVar).asInstanceOf[Factor[T]]
    if (factor.size != 1) throw new AlgorithmException//("Final factor for most likely value has more than one entry")
    factor.get(List())
  }
}

trait OneTimeStructuredMPE extends StructuredMPEAlgorithm with OneTimeStructured with OneTimeMPE

trait AnytimeStructuredMPE extends StructuredMPEAlgorithm with AnytimeStructured with AnytimeMPE
