package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language.Atomic
import com.cra.figaro.language.ElementCollection
import com.cra.figaro.language.Element
import com.cra.figaro.language.Name
import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import scala.math.{ exp, log, pow }
import JSci.maths.SpecialMath.{ gamma, logGamma }
/**
 * A Gamma distribution in which both the k and theta parameters are constants.
 * Theta defaults to 1.
 */
class AtomicInverseGamma(name: Name[Double], shape: Double, scale: Double = 1.0, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] {


  
  
  type Randomness = Double

  def generateRandomness() = Util.generateGamma(shape)

  def generateValue(rand: Randomness) = 1.0 / (rand * scale) // due to scaling property of Gamma

  /**
   * The normalizing factor.
   */
  private val normalizer = pow(scale, shape) / (gamma(shape))

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
  def apply(shape: Double, scale: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicInverseGamma(name, shape, scale, collection)

}