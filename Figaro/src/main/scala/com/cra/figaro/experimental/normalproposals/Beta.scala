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

package com.cra.figaro.experimental.normalproposals

import com.cra.figaro.language._

import math.{ pow, sqrt }
import JSci.maths.SpecialMath.beta
import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.library.atomic.continuous.Util

/**
 * A Beta distribution in which the alpha and beta parameters are provided.
 * This Beta element can be used as the parameter for a ParameterizedFlip.
 *
 * @param a The prior alpha parameter
 * @param b The prior beta parameter
 */
class AtomicBeta(name: Name[Double], a: Double, b: Double, collection: ElementCollection)
  extends Element[Double](name, collection) with HasDensity[Double] with NormalProposer with DoubleParameter with com.cra.figaro.library.atomic.continuous.Beta {
  // Bounds for normal proposals
  override def lower = 0.0
  override def upper = 1.0
  // Proposal scale is 20% of the standard deviation
  def proposalScale = 0.2 * sqrt(a * b / (a + b + 1)) / (a + b)

  override def nextRandomness(oldRandomness: Randomness): (Randomness, Double, Double) = {
    // If a or b is greater than 1, the distribution is unimodal, so normal proposals are appropriate
    if(a >= 1 || b >= 1) super[NormalProposer].nextRandomness(oldRandomness)
    // If both a and b are less than 1, the distribution is bimodal, so we're better off proposing from the prior
    else super[HasDensity].nextRandomness(oldRandomness)
  }

  /**
   * The learned alpha parameter
   */
  var learnedAlpha = a
  /**
   * The learned beta parameter
   */
  var learnedBeta = b
  def aValue = learnedAlpha
  def bValue = learnedBeta
  def generateRandomness() = Util.generateBeta(a, b)

  /**
   * The normalizing factor.
   */
  val normalizer = 1 / beta(a, b)

  /**
   * Density of a value.
   */
  def density(x: Double) = pow(x, a - 1) * pow(1 - x, b - 1) * normalizer

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

object Beta {
  /**
   * Create a Beta distribution in which the parameters are constants.
   */
  def apply(a: Double, b: Double)(implicit name: Name[Double], collection: ElementCollection) =
  new AtomicBeta(name, a, b, collection)
}
