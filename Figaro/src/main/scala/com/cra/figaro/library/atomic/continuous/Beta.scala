/*
 * Beta.scala
 * Elements representing Beta distributions.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 2, 2011
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._
import math.pow
import JSci.maths.SpecialMath.{ beta, gamma }
import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.ValueSet

/**
 * A Beta distribution in which the alpha and beta parameters are provided.
 * This Beta element can be used as the parameter for a ParameterizedFlip.
 * 
 * @param a The prior alpha parameter
 * @param b The prior beta parameter
 */
class AtomicBeta(name: Name[Double], a: Double, b: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with Atomic[Double] with Parameter[Double] with ValuesMaker[Double] {
  type Randomness = Double

  def generateRandomness() = Util.generateBeta(a, b)

  def generateValue(rand: Randomness) = rand

  /**
   * The normalizing factor.
   */
  val normalizer = 1 / beta(a, b)

  /**
   * Density of a value.
   */
  def density(x: Double) = if (x < 0.0 || x > 1.0) 0.0 else pow(x, a - 1) * pow(1 - x, b - 1) * normalizer

  /**
   * The learned alpha parameter
   */
  var learnedAlpha = a
  /**
   * The learned beta parameter
   */
  var learnedBeta = b
  
  /**
   * Returns an empty sufficient statistics vector. 
   */
  override def zeroSufficientStatistics(): Seq[Double] = {
      Seq(0.0, 0.0)
    }

  /**
   * Returns an element that models the learned distribution.
   * 
   * @deprecated
   */
  def getLearnedElement: AtomicFlip = {
      new AtomicFlip("", MAPValue, collection)
    }

  override def sufficientStatistics[Boolean](b: Boolean): Seq[Double] = {
    if (b == true) {
      Seq(1.0, 0.0)
    } else {
      Seq(0.0, 1.0)
    }
  }
  
  private[figaro] override def sufficientStatistics[Boolean](i: Int): Seq[Double] = {
    if (i == 0) {
      Seq(1.0, 0.0)
    } else {
      Seq(0.0, 1.0)
    }
  }


  def expectedValue: Double = {
    (learnedAlpha) / (learnedAlpha + learnedBeta)
  }
  
  def MAPValue: Double = {
    if (learnedAlpha + learnedBeta == 2) 0.5
    else (learnedAlpha - 1) / (learnedAlpha + learnedBeta - 2)
  }

  def makeValues(depth: Int) = ValueSet.withoutStar(Set(MAPValue))

  def maximize(sufficientStatistics: Seq[Double]) {

    require(sufficientStatistics.size == 2)
    learnedAlpha = sufficientStatistics(0) + a
    learnedBeta = sufficientStatistics(1) + b

  }

  override def toString = "Beta(" + a + ", " + b + ")"
}

/**
 * A Beta distribution in which the parameters are elements.
 */
class CompoundBeta(name: Name[Double], a: Element[Double], b: Element[Double], collection: ElementCollection)
  extends NonCachingChain[Double, Double](
    name,
    a,
    (a: Double) => new NonCachingChain(
      "",
      b,
      (b: Double) => new AtomicBeta("", a, b, collection),
      collection),
    collection) {
  override def toString = "Beta(" + a + ", " + b + ")"
}

object Beta extends Creatable {
  /**
   * Create a Beta distribution in which the parameters are constants.
   */
  def apply(a: Double, b: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new AtomicBeta(name, a, b, collection)

  /**
   * Create a Beta distribution in which the parameters are elements.
   */
  def apply(a: Element[Double], b: Element[Double])(implicit name: Name[Double], collection: ElementCollection) =
    new CompoundBeta(name, a, b, collection)

  type ResultType = Double
  
  def create(args: List[Element[_]]) = apply(args(0).asInstanceOf[Element[Double]], args(1).asInstanceOf[Element[Double]])
}
