/*
 * NormalProposer.scala
 * Normally distributed proposals.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 17, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.normalproposals

import com.cra.figaro.language._
import com.cra.figaro.util._
import org.apache.commons.math3.distribution.NormalDistribution

import scala.annotation.tailrec

/**
 * Normally distributed proposals for univariate continuous elements. This works by proposing from a truncated normal
 * distribution over the randomness of this element.
 */
trait NormalProposer extends Atomic[Double] {
  type Randomness = Double

  /**
   * Inclusive lower bound of the range of the randomness of this element. Defaults to -Infinity. Must be strictly less
   * than the upper bound.
   */
  def lower: Double = Double.NegativeInfinity
  /**
   * Exclusive upper bound of the range of the randomness of this element. Defaults to Infinity. Must be strictly
   * greater than the lower bound.
   */
  def upper: Double = Double.PositiveInfinity

  /**
   * The scale of the normally distributed proposal. This corresponds to the standard deviation of the proposal before
   * truncation. If the randomness has finite variance, this should be less than or equal to its standard deviation.
   * A good place to start is e.g. 20% of the standard deviation.
   */
  def proposalScale: Double

  /**
   * A strictly monotone differentiable function defined on [`lower`, `upper`). Defaults to the identity function.
   */
  def generateValue(rand: Randomness) = rand

  /**
   * The absolute value of the derivative of `generateValue` with respect to the randomness given. This is needed to
   * compute a proposal density over the transformed randomness. Defaults to 1.0, corresponding to the case where
   * `generateValue` is the identity function.
   */
  def generateValueDerivative(rand: Randomness): Double = 1.0

  /**
   * Generate the next randomness given the current randomness.
   * Returns three values: The next randomness, the Metropolis-Hastings proposal probability
   * ratio, which is:
   *
   * P(new -> old) / P(old -> new)
   *
   * and the model probability ratio, which is:
   *
   * P(new) / P(old)
   *
   * This implementation proposes a normally distributed randomness from the previous randomness, truncated to be within
   * the range of this element. The probability ratios returned are associated with the values of this element rather
   * than the randomness.
   */
  @tailrec
  override final def nextRandomness(oldRandomness: Randomness): (Randomness, Double, Double) = {
    /*
     * Sample from a normal distribution centered at oldRandomness, and reject if it's outside the range.
     * Rejection sampling to be in the range is justified here because of the assumptions on the scale.
     * If both lower and upper bounds are finite, then the scale is less than the maximum possible standard deviation of
     * (upper - lower) / 2. This yields a minimum acceptance probability of ~47.72% (cumulative density of a standard
     * normal distribution from 0 to 2) when oldRandomness is one of the bounds.
     * If only one of the bounds is finite, then we reject less than 50% of the time, since no more than half of the
     * distribution is truncated by the bounds. If the distribution is unbounded, we never reject.
     * So, the expected number of trials is never more than 1 / 0.4772 =  2.095, which is quite reasonable.
     */
    val newRandomness = oldRandomness + random.nextGaussian() * proposalScale
    if(lower <= newRandomness && newRandomness < upper) {
      val proposalRatio = proposalProb(newRandomness, oldRandomness) / proposalProb(oldRandomness, newRandomness)
      val modelRatio = density(generateValue(newRandomness)) / density(generateValue(oldRandomness))
      (newRandomness, proposalRatio, modelRatio)
    }
    else nextRandomness(oldRandomness)
  }

  /**
   * Computes the proposal probability density in one direction. Both values should be in the interval [lower, upper).
   * @param oldRandomness The previous randomness.
   * @param newRandomness The newly proposed randomness.
   * @return The probability density associated with the transition from `generateValue(oldRandomness)` to
   * `generateValue(newRandomness)`. This is a density over the corresponding values (as opposed to randomnesses).
   */
  private def proposalProb(oldRandomness: Double, newRandomness: Double): Double = {
    val dist = new NormalDistribution(oldRandomness, proposalScale)
    // The probability density of proposing the new value given the old value is the density of the new value from the
    // normal distribution, divided by the normalizing constant of the cumulative probability between the upper and
    // lower bounds. This ensures that the PDF of the truncated distribution integrates to 1.
    val uncorrected = dist.probability(newRandomness) / dist.probability(lower, upper)
    // Correct for the scaling factor associated with newRandomness.
    uncorrected / generateValueDerivative(newRandomness)
  }
}
