/*
 * Gamma.scala
 * Elements representing Gamma elements.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 25, 2011
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.normalproposals

import JSci.maths.SpecialMath.gamma
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Util

import scala.math._

/**
 * A Gamma distribution in which both the k and theta parameters are constants.
 * Theta defaults to 1.
 */
class AtomicGamma(name: Name[Double], k: Double, theta: Double = 1.0, collection: ElementCollection)
  extends Element[Double](name, collection) with HasDensity[Double] with NormalProposer {
  // Lower bound for normal proposals
  override def lower = 0.0
  // Proposal scale is 20% of the standard deviation of the underlying Gamma randomness
  def proposalScale = 0.2 * sqrt(k)

  def generateRandomness() = Util.generateGamma(k)

  override def generateValue(rand: Randomness) =
    rand * theta // due to scaling property of Gamma

  override def generateValueDerivative(rand: Randomness) = theta

  override def nextRandomness(oldRandomness: Randomness): (Randomness, Double, Double) = {
    // If k is large, then the density is spread out enough for normal proposals to work reasonably well.
    if(k >= 1) super[NormalProposer].nextRandomness(oldRandomness)
    // If k is small, too much density is concentrated extremely close to 0 for normal proposals to be effective.
    // For example, if k=0.1, the median is ~0.00059.
    else super[HasDensity].nextRandomness(oldRandomness)
  }

  /**
   * The normalizing factor.
   */
  private val normalizer = 1.0 / (gamma(k) * pow(theta, k))

  /**
   * Density of a value.
   */
  def density(x: Double) = {
    if (x < 0.0) 0.0 else {
      val numer = pow(x, k - 1) * exp(-x / theta)
      numer * normalizer
    }
  }

  override def toString =
    if (theta == 1.0) "Gamma(" + k + ")"
    else "Gamma(" + k + ", " + theta + ")"
}

object Gamma {
  /**
   * Create a Gamma element in which both k and theta parameters are constants. Theta defaults to 1.
   */
  def apply(k: Double, theta: Double = 1.0)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicGamma(name, k, theta, collection)
}
