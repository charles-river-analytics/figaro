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
import com.cra.figaro.util.random
import scala.math._

/**
 * A normal distribution in which the mean and variance are constants.
 */
class AtomicNormal(name: Name[Double], val mean: Double, val variance: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] with Normal {
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
  def density(d: Double) = {
    val diff = d - mean
    val exponent = -(diff * diff) / (2.0 * variance)
    normalizer * exp(exponent)
  }

  override def toString = "Normal(" + mean + ", " + variance + ")"

  lazy val constraint = Constant((mean, variance))
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
  override def toString = "Normal(" + mean + ", " + variance + ")"

  lazy val constraint = Apply(mean, (m: Double) => (m,variance))
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
  override def toString = "Normal(" + mean + ", " + variance + ")"

  lazy val constraint = Apply(mean, variance, (m: Double, v: Double) => (m, v))
}

trait Normal extends Element[Double] {

  def constraint: Element[(Double,Double)]

  override def observe(observation: Double) {
    constraint.addLogConstraint { e: (Double, Double) =>
      val (m, v) = e
      (- (observation - m) * (observation - m) / v + log(1 / Pi / 2.0 / v)) / 2.0
    }
    this.deactivate()
  }

  override def unobserve() {
    constraint.deactivate()
    this.activate()
  }

}

object Normal extends Creatable {
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
   * Create a normal distribution in both the mean and the variance are constants.
   */
  def apply(mean: Element[Double], variance: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundNormal(name, mean, variance, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]], args(1).asInstanceOf[Element[Double]])
}
