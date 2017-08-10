/*
 * Util.scala
 * Utility functions for atomic discrete elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.util._
import scala.math.{ ceil, log }

object Util {
  /**
   * Generate a geometric distributed random variable.
   */
  def generateGeometric(probFail: Double) =
    ceil(log(random.nextDouble()) / log(probFail)).toInt

  /**
   * Density of the given number of positive outcomes under a binomial random variable with the given number of trials.
   * Computing a binomial coefficient exactly can be very expensive for a large number of trials, so this method uses
   * an approximation algorithm when the number of trials is sufficiently large.
   */  
  def binomialDensity(numTrials: Int, probSuccess: Double, numPositive: Int): Double = {
    val q = 1 - probSuccess
    if (numTrials > 10) {
      val logNFact = JSci.maths.ExtraMath.logFactorial(numTrials)
      val logKFact = JSci.maths.ExtraMath.logFactorial(numPositive)
      val logNMinusKFact = JSci.maths.ExtraMath.logFactorial(numTrials-numPositive)
      val logBinomialCoefficient = logNFact - (logKFact + logNMinusKFact)
      val result = logBinomialCoefficient + (numPositive*Math.log(probSuccess) + ((numTrials-numPositive)*Math.log(q)))
      Math.exp(result)
    } else {
      JSci.maths.ExtraMath.binomial(numTrials, numPositive) * math.pow(probSuccess, numPositive) * math.pow(q, numTrials - numPositive)
    }
  }
}

