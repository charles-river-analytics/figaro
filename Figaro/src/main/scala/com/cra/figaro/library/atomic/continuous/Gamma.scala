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

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import scala.math._
import JSci.maths.SpecialMath.gamma

/**
 * A Gamma distribution in which both the k and theta parameters are constants.
 * Theta defaults to 1.
 */
class AtomicGamma(name: Name[Double], k: Double, theta: Double = 1.0, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] {
  type Randomness = Double

  def generateRandomness() = Util.generateGamma(k)

  def generateValue(rand: Randomness) =
    rand * theta // due to scaling property of Gamma

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

/**
 * A Gamma distribution in which the k parameter is an element and theta is constant. Theta defaults to 1.
 */
class GammaCompoundK(name: Name[Double], k: Element[Double], theta: Double = 1.0, collection: ElementCollection)
  extends NonCachingChain(
    name,
    k,
    (k: Double) => new AtomicGamma("", k, theta, collection),
    collection) {
  override def toString =
    if (theta == 1.0) "Gamma(" + k + ")"
    else "Gamma(" + k + ", " + theta + ")"
}

/**
 * A Gamma distribution in which k and theta are both elements.
 */
class CompoundGamma(name: Name[Double], k: Element[Double], theta: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Double](
    name,
    k,
    (k: Double) => new NonCachingChain(
      "",
      theta,
      (t: Double) => new AtomicGamma("", k, t, collection),
      collection),
    collection) {
  override def toString = "Gamma(" + k + ", " + theta + ")"
}

object Gamma extends Creatable {
  /**
   * Create a Gamma element in which both k and theta parameters are constants. Theta defaults to 1.
   */
  def apply(k: Double, theta: Double = 1.0)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicGamma(name, k, theta, collection)

  /**
   * Create a Gamma element in which the k parameter is an element and theta is a constant.
   * Theta defaults to 1.
   */
  def apply(k: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new GammaCompoundK(name, k, 1.0, collection)

  /**
   * Create a Gamma element in which both the k and theta parameters are elements.
   */
  def apply(k: Element[Double], theta: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundGamma(name, k, theta, collection)

  type ResultType = Double

  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]], args(1).asInstanceOf[Element[Double]])
}
