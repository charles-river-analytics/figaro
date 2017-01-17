/*
 * Uniform.scala
 * Elements representing continuous uniform distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 18, 2010
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import com.cra.figaro.util.{ random, bound }
import scala.math.log

/**
 * A continuous uniform distribution in which the parameters are constants.
 */
class AtomicUniform(name: Name[Double], val lower: Double, val upper: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] with Uniform {
  type Randomness = Double
  
  def lowerValue: Double = lower
  
  def upperValue: Double = upper
  
  private lazy val diff = upper - lower

  def generateRandomness() = random.nextDouble() * diff + lower

  def generateValue(rand: Randomness) = rand

  private lazy val constantDensity = 1.0 / diff

  def density(d: Double) = if (d >= lower && d < upper) constantDensity; else 0.0

  override def toString = "Uniform(" + lower + ", " + upper + ")"
}

/**
 * A continuous uniform distribution in which the parameters are elements.
 */
class CompoundUniform(name: Name[Double], val lower: Element[Double], val upper: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Double](
    name,
    lower,
    (l: Double) => new NonCachingChain(
      "",
      upper,
      (u: Double) => new AtomicUniform("", l, u, collection),
      collection),
    collection)
  with Uniform {

  def lowerValue = lower.value
  def upperValue = upper.value

  override def toString = "Uniform(" + lower.toString + ", " + upper.toString + ")"
}

trait Uniform extends Continuous[Double] {

  /**
   * Current lower value.
   */
  def lowerValue: Double

  /**
   * Current upper value.
   */
  def upperValue: Double

  def logp(value: Double) =
    bound (
      -log(upperValue - lowerValue),
      lowerValue > 0,
      upperValue > 0
    )

}

object Uniform extends Creatable {
  /**
   * Create a continuous uniform distribution in which the parameters are constants.
   */
  def apply(lower: Double, upper: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicUniform(name, lower, upper, collection)

  /**
   * Create a continuous uniform distribution in which the parameters are elements.
   */
  def apply(lower: Element[Double], upper: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundUniform(name, lower, upper, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]], args(1).asInstanceOf[Element[Double]])
}
