/*
 * MultivariateNormal.scala
 * Elements representing a multivariate normal distributions
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jun 2, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import com.cra.figaro.util.random
import scala.math._
import scala.collection.JavaConversions
import org.apache.commons.math3.distribution.MultivariateNormalDistribution

/**
 * A multivariate normal distribution in which the means and variance-covariances are constants.
 */
class AtomicMultivariateNormal(name: Name[List[Double]], val means: List[Double], val covariances: List[List[Double]], collection: ElementCollection)
  extends Element[List[Double]](name, collection) with Atomic[List[Double]] {
  
  val distribution = new MultivariateNormalDistribution(means.toArray, covariances.map((l: List[Double]) => l.toArray).toArray)
  
  lazy val standardDeviations = distribution.getStandardDeviations()

  type Randomness = List[Double]

  def generateRandomness(): List[Double] = {
    distribution.sample.toList
  }

  def generateValue(rand: Randomness) = rand

  /**
   * Density of a value.
   */
  def density(d: List[Double]) = {
   distribution.density(d.toArray)
  }

  override def toString = "MultivariateNormal(" + means + ",\n" + covariances + ")"
}

/**
 * A normal distribution in which the mean is an element and the variance is constant.
 */
class MultivariateNormalCompoundMean(name: Name[List[Double]], val means: Element[List[Double]], val covariances: List[List[Double]], collection: ElementCollection)
  extends NonCachingChain(
    name,
    means,
    (m: List[Double]) => new AtomicMultivariateNormal("", m, covariances, collection),
    collection) {
  override def toString = "Normal(" + means + ",\n " + covariances + ")"
}

/**
 * A normal distribution in which the mean and variance are both elements.
 */
class MultivariateCompoundNormal(name: Name[List[Double]], val mean: Element[List[Double]], val variance: Element[List[List[Double]]], collection: ElementCollection)
  extends NonCachingChain[List[Double], List[Double]](
    name,
    mean,
    (m: List[Double]) => new NonCachingChain(
      "",
      variance,
      (v: List[List[Double]]) => new AtomicMultivariateNormal("", m, v, collection),
      collection),
    collection) {
  override def toString = "Normal(" + mean + ", " + variance + ")"
}

object MultivariateNormal extends Creatable {
  /**
   * Create a normal distribution in which the mean and variance are constants.
   */
  def apply(means: List[Double], covariances: List[List[Double]])(implicit name: Name[List[Double]], collection: ElementCollection) =
    new AtomicMultivariateNormal(name, means, covariances, collection)

  /**
   * Create a normal distribution in which the mean is an element and the variance is constant.
   */
  def apply(means: Element[List[Double]], covariances: List[List[Double]])(implicit name: Name[List[Double]], collection: ElementCollection) =
    new MultivariateNormalCompoundMean(name, means, covariances, collection)

  /**
   * Create a normal distribution in both the mean and the variance are elements.
   */
  def apply(mean: Element[List[Double]], variance: Element[List[List[Double]]])(implicit name: Name[List[Double]], collection: ElementCollection) =
    new MultivariateCompoundNormal(name, mean, variance, collection)

  type ResultType = List[Double]

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[List[Double]]], args(1).asInstanceOf[Element[List[List[Double]]]])
}
