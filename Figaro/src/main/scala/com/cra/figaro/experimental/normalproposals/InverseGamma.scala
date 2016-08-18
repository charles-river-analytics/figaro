/*
 * InverseGamma.scala
 * Class for a Gamma distribution in which both the k and theta parameters are constants
 *
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Dec 4, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
class AtomicInverseGamma(name: Name[Double], shape: Double, scale: Double = 1.0, collection: ElementCollection)
  extends Element[Double](name, collection) with NormalProposer {
  // Lower bound for normal proposals
  override def lower = 0.0
  // Proposal scale is 20% of the standard deviation of the underlying Gamma randomness
  def proposalScale = 0.2 * sqrt(shape)

  def generateRandomness() = Util.generateGamma(shape)

  override def generateValue(rand: Randomness) = 1.0 / (rand * scale) // due to scaling property of Gamma

  override def generateValueDerivative(rand: Randomness) = 1.0 / (rand * rand * scale)

  /**
   * The normalizing factor.
   */
  private val normalizer = pow(scale, shape) / gamma(shape)

  /**
   * Density of a value.
   */
  def density(x: Double) = {
    if (x < 0.0) 0.0 else {
      //Convert to logarithms if this is too large.
      val numer = pow(x, -1.0 * shape - 1) * exp(-1.0 * scale / x)
      numer * normalizer
    }
  }

  override def toString =
    if (scale == 1.0) "InverseGamma(" + shape + ")"
    else "InverseGamma(" + shape + ", " + scale + ")"
}

object InverseGamma {
    /**
   * Create an InverseGamma element.
   */
  def apply(shape: Double, scale: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicInverseGamma(name, shape, scale, collection)

}