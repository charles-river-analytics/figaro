/*
 * Normal.scala
 * Elements representing normal distributions
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.normalproposals

import com.cra.figaro.language._
import com.cra.figaro.util.{bound, random}

import scala.math._

/**
 * A normal distribution in which the mean and variance are constants.
 */
class AtomicNormal(name: Name[Double], val mean: Double, val variance: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with NormalProposer {
  lazy val standardDeviation = sqrt(variance)

  // Proposal scale is 20% of the standard deviation of the underlying standard Normal randomness
  def proposalScale = 0.2

  def generateRandomness() = {
    val u1 = random.nextDouble()
    val u2 = random.nextDouble()
    val w = sqrt(-2.0 * log(u1))
    val x = 2.0 * Pi * u2
    w * sin(x)
  }

  override def generateValue(rand: Randomness) = rand * standardDeviation + mean

  override def generateValueDerivative(rand: Randomness) = standardDeviation

  /**
   * The normalizing factor.
   */
  private val normalizer = 1.0 / sqrt(2.0 * Pi * variance)

  /**
   * Density of a value.
   */
  def density(d: Double) = Normal.density(mean, variance, normalizer)(d)

  override def toString = "Normal(" + mean + ", " + variance + ")"
}

object Normal {

  def density(mean: Double, variance: Double, normalizer: Double)(d: Double) = {
    val diff = d - mean
    val exponent = -(diff * diff) / (2.0 * variance)
    normalizer * exp(exponent)
  }

  /**
   * Create a normal distribution in which the mean and variance are constants.
   */
  def apply(mean: Double, variance: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicNormal(name, mean, variance, collection)
}
