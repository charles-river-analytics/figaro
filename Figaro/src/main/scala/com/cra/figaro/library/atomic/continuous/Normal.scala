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

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import com.cra.figaro.util.{ random, bound }
import scala.math._

/**
 * A normal distribution in which the mean and variance are constants.
 */
class AtomicNormal(name: Name[Double], val mean: Double, val variance: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] {
  lazy val standardDeviation = sqrt(variance)

  type Randomness = Double

  def generateRandomness() = {
    val u1 = random.nextDouble()
    val u2 = random.nextDouble()
    val w = sqrt(-2.0 * log(u1))
    val x = 2.0 * Pi * u2
    w * sin(x)
  }

  def generateValue(rand: Randomness) = rand * standardDeviation + mean

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

/**
 * A normal distribution in which the mean is an element and the variance is constant.
 */
class NormalCompoundMean(name: Name[Double], val mean: Element[Double], val variance: Double, collection: ElementCollection)
  extends NonCachingChain(
    name,
    mean,
    (m: Double) => new AtomicNormal("", m, variance, collection),
    collection)
  with Normal {

  def meanValue = mean.value
  lazy val varianceValue = variance

  override def toString = "Normal(" + mean + ", " + variance + ")"
}


/**
 * A normal distribution in which the mean is constant and the variance is an element.
 */
class NormalCompoundVariance(name: Name[Double], val mean: Double, val variance: Element[Double], collection: ElementCollection)
  extends NonCachingChain(
    name,
    variance,
    (v: Double) => new AtomicNormal("", mean, v, collection),
    collection)
  with Normal {

  def varianceValue = variance.value
  lazy val meanValue = mean

  override def toString = "Normal(" + mean + ", " + variance + ")"
}

/**
 * A normal distribution in which the mean and variance are both elements.
 */
class CompoundNormal(name: Name[Double], val mean: Element[Double], val variance: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Double](
    name,
    mean,
    (m: Double) => new NonCachingChain(
      "",
      variance,
      (v: Double) => new AtomicNormal("", m, v, collection),
      collection),
    collection)
  with Normal {

  def meanValue = mean.value
  def varianceValue = variance.value

  override def toString = "Normal(" + mean + ", " + variance + ")"
}

trait Normal extends Continuous[Double] {

  /**
   * Current mean value.
   */
  def meanValue: Double

  /**
   * Current variance value.
   */
  def varianceValue: Double

  def logp(value: Double) =
    bound(
      (- (value - meanValue) * (value - meanValue) / varianceValue + log(1 / Pi / 2.0 / varianceValue)) / 2.0,
      varianceValue > 0
    )

}

object Normal extends Creatable {
  
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

  /**
   * Create a normal distribution in which the mean is an element and the variance is constant.
   */
  def apply(mean: Element[Double], variance: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new NormalCompoundMean(name, mean, variance, collection)

    /**
   * Create a normal distribution in which the mean is an constant and the variance is an element.
   */
  def apply(mean: Double, variance: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new NormalCompoundVariance(name, mean, variance, collection)
  
  
  /**
   * Create a normal distribution in both the mean and the variance are elements.
   */
  def apply(mean: Element[Double], variance: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundNormal(name, mean, variance, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]], args(1).asInstanceOf[Element[Double]])
}
