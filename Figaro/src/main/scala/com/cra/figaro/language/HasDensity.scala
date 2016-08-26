/*
 * HasDensity.scala
 * Trait elements that have a density defined.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jun 10, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Trait of elements for which a density method is defined.
 */
trait HasDensity[T] extends Element[T] {
  /** The probability density of a value. */
  def density(t: T): Double

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
   * This implementation produces a sample using generateRandomness, which means that:
   *
   * P(new -> old) / P(old -> new) = P(old) / P(new)
   *
   * We use the fact that this element can compute densities for values to compute P(new) and
   * P(old) explicitly. Note that the two returned ratios will still multiply to 1. This does
   * not affect normal Metropolis-Hastings, but helps the Metropolis-Hastings annealer find
   * maxima.
   */
  override def nextRandomness(oldRandomness: Randomness): (Randomness, Double, Double) = {
    val newRandomness = generateRandomness()
    val pOld = density(generateValue(oldRandomness))
    val pNew = density(generateValue(newRandomness))
    // Note that these density ratios could overflow/underflow, particularly if there is nonzero probability of
    // generating a value that has infinite density. For example, this may occur when generating from a Gamma
    // distribution with sufficiently small shape parameter such that the value 0 may be produced, which has infinite
    // density. We don't account for this explicitly because MH is unlikely to work well for these models to begin with.
    (newRandomness, pOld / pNew, pNew / pOld)
  }
}
