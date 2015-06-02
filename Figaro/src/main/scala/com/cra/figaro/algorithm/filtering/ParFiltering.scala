/*
 * Filtering.scala
 * Filtering algorithms.
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
 *  A parallel version of Filtering. Specifically a version of OneTimeFiltering, since that's the only 
 *  target for parallelization right now.
 */
abstract class ParFiltering(transition: (Universe, Universe) => Universe) extends Algorithm with OneTime {

  /**
   * Returns the probability that the element referred to by the reference
   * produces the given value at the current time point.
   */
  def currentProbability[T](reference: Reference[T], value: T): Double =
    currentProbability(reference, (t: T) => t == value)
  
  /* One-time implementations */
  
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

  /* The following four methods must be defined by any implementation of filtering. */

  /**
   * Advance the filtering one time step, conditioning on the given evidence at the new time point.
   */
  def advanceTime(evidence: Seq[NamedEvidence[_]]): Unit

  /**
   * Returns the distribution over the element referred to by the reference at the current time point.
   */
  protected def computeCurrentDistribution[T](reference: Reference[T]): Stream[(Double, T)]

  /**
   * Returns the expectation of the element referred to by the reference
   * under the given function at the current time point.
   */
  protected def computeCurrentExpectation[T](reference: Reference[T], function: T => Double): Double

  /**
   * Returns the probability that the element referred to by the reference
   * satisfies the given predicate at the current time point.
   */
  protected def computeCurrentProbability[T](reference: Reference[T], predicate: T => Boolean): Double =
    computeCurrentExpectation(reference, (t: T) => if (predicate(t)) 1.0; else 0.0)
}
