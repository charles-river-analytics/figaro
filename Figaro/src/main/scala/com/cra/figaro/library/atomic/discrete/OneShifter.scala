/*
 * OneShifter.scala
 * Atomic elements with integer randomness whose proposal distribution shifts the value by 1.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 17, 2011
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.discrete

import com.cra.figaro.language._
import com.cra.figaro.util._

/**
 * Atomic elements over integers with integer randomness whose proposal distribution shifts the value by 1.
 */
trait OneShifter extends Atomic[Int] {
  type Randomness = Int

  protected val lowerBound: Int
  protected val upperBound: Int

  private lazy val density0 = density(lowerBound)
  private lazy val density1 = density(lowerBound + 1)
  private lazy val density2 = density(lowerBound + 2)
  private lazy val density3 = density(lowerBound + 3)
  private lazy val prob10 = density0 / (density0 + density2)
  private lazy val prob12 = 1.0 - prob10
  private lazy val prob21 = density1 / (density1 + density3)


  private lazy val densityU0 = density(upperBound)
  private lazy val densityU1 = density(upperBound - 1)
  private lazy val densityU2 = density(upperBound - 2)
  private lazy val densityU3 = density(upperBound - 3)
  private lazy val probU10 = densityU0 / (densityU0 + densityU2)
  private lazy val probU12 = 1.0 - probU10
  private lazy val probU21 = densityU1 / (densityU1 + densityU3)


  /**
   * Shift the randomness by one. Returns the new randomness,
   * the Metropolis-Hastings proposal ratio, and the model ratio.
   * Ensures that the randomness remains between the lower and upper bound.
   */
  def shiftOne(rand: Int): (Int, Double, Double) = {
    if (rand == lowerBound) {
      (lowerBound + 1, prob10, density1 / density0) // automatically go up
    } else if (rand == lowerBound + 1) {
      if (random.nextDouble < prob12) (lowerBound + 2, prob21 / prob12, density2 / density1)
      else (lowerBound, 1.0 / prob10, density0 / density1)
    } else if (rand == upperBound) {
      (upperBound - 1, probU10, densityU1 / densityU0) // automatically go up
    } else if (rand == upperBound - 1) {
      if (random.nextDouble < probU12) (upperBound - 2, probU21 / probU12, densityU2 / densityU1)
      else (upperBound, 1.0 / probU10, densityU0 / densityU1)
    } else {
      val densityThis = density(rand)
      val densityUp = density(rand + 1)
      val densityDown = density(rand - 1)
      val probDown = densityDown / (densityUp + densityDown)
      val probUp = 1.0 - probDown
      if (random.nextDouble() < probUp) {
        val densityUp2 = density(rand + 2)
        val probUpDown = densityThis / (densityThis + densityUp2)
        val proposalProb = probUpDown / probUp
        val modelProb = densityUp / densityThis
        (rand + 1, proposalProb, modelProb)
      } else {
        val densityDown2 = density(rand - 2)
        val probDownUp = densityThis / (densityThis + densityDown2)
        val ratio = probDownUp * densityDown / (probDown * densityThis)
        val proposalProb = probDownUp / probDown
        val modelProb = densityDown / densityThis
        (rand - 1, proposalProb, modelProb)
      }
    }
  }
}
