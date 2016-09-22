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
 * distribution over the randomness of this element. This implementation assumes that the probability density of values
 * associated with randomnesses in the range (`lower`, `upper`) are finite.
 */
trait NormalProposer extends Atomic[Double] {
  type Randomness = Double

  /**
   * Exclusive lower bound of the range of the randomness of this element. Defaults to -Infinity. Must be strictly less
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
   * A strictly monotone differentiable function defined on (`lower`, `upper`). Defaults to the identity function.
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
   * By default, this implementation proposes a normally distributed randomness from the previous randomness, truncated
   * to be within the appropriate range. The probability ratios returned are associated with the values of this element
   * rather than the randomness. This is for the purpose of simulated annealing, since the most likely randomness is not
   * necessarily the most likely value, depending on the form of the generateValue function.
   *
   * One can override this to only use normal proposals in certain special cases.
   */
  override def nextRandomness(oldRandomness: Randomness): (Randomness, Double, Double) = {
    normalNextRandomness(oldRandomness)
  }

  /**
   * Computes the normal proposal for nextRandomness. This is separated from the nextRandomness so that subclasses can
   * choose when to use normal proposals.
   */
  @tailrec
  protected final def normalNextRandomness(oldRandomness: Randomness): (Randomness, Double, Double) = {
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
    if(lower < newRandomness && newRandomness < upper) {
      val proposalRatio = proposalProb(newRandomness, oldRandomness) / proposalProb(oldRandomness, newRandomness)
      val modelRatio = density(generateValue(newRandomness)) / density(generateValue(oldRandomness))
      (newRandomness, proposalRatio, modelRatio)
    }
    else normalNextRandomness(oldRandomness)
  }

  /**
   * Computes the proposal probability density in one direction. Both values should be in the interval (lower, upper).
   * @param oldRandomness The previous randomness.
   * @param newRandomness The newly proposed randomness.
   * @return The probability density associated with the transition from `generateValue(oldRandomness)` to
   * `generateValue(newRandomness)`. This is a density over the corresponding values (as opposed to randomnesses).
   */
  protected def proposalProb(oldRandomness: Double, newRandomness: Double): Double = {
    val stDev = proposalScale

    // The probability density of proposing the new randomness given the old randomness is the density of the new
    // randomness from the normal distribution, divided by the normalizing constant of the cumulative probability
    // between the upper and lower bounds. This cumulative probability corresponds to the probability of not rejecting
    // when we sample from the regular normal proposal. This ensures that the PDF of the truncated distribution
    // integrates to 1.
    val uncorrected =
      Normal.density(oldRandomness, stDev)(newRandomness) / Normal.probability(oldRandomness, stDev)(lower, upper)

    // Correct for the scaling factor associated with newRandomness. This is needed because even though the proposal
    // distribution over the randomness of this element is a truncated normal distribution, the resulting proposal
    // distribution over the actual values of this element may be different. Consider, for example, how an inverse Gamma
    // element might use a Gamma distribution as its randomness, then invert the randomness to produce a value. Then, a
    // truncated normal distribution over the randomness would have some other distribution over the values. Since we
    // have the restriction that the deterministic transformation function is strictly monotone and differentiable, we
    // can account for this difference; this is a standard "change of variable" computation.
    uncorrected / generateValueDerivative(newRandomness)
  }
}
