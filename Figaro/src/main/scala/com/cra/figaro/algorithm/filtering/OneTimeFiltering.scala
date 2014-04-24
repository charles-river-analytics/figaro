/*
 * OneTimeFiltering.scala
 * One-time filtering algorithms.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.algorithm._
import com.cra.figaro.language._

/**
 * One-time filtering algorithms. An implementation of OneTimeFiltering must implement the advanceTime,
 * computeCurrentDistribution, and computeCurrentExpectation methods.
 */
trait OneTimeFiltering extends Filtering with OneTime {
  /**
   * Returns the distribution over the element referred to by the reference at the current time point.
   */
  def currentDistribution[T](reference: Reference[T]): Stream[(Double, T)] =
    computeCurrentDistribution(reference)

  /**
   * Returns the expectation of the element referred to by the reference
   * under the given function at the current time point.
   */
  def currentExpectation[T](reference: Reference[T], function: T => Double): Double =
    computeCurrentExpectation(reference, function)
    

  /**
   * Returns the probability that the element referred to by the reference
   * satisfies the given predicate at the current time point.
   */
  def currentProbability[T](reference: Reference[T], predicate: T => Boolean): Double =
    computeCurrentProbability(reference, predicate)
}
