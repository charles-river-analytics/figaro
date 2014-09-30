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
 * The general class of filtering algorithms. A filtering algorithm is provided with an initial model,
 * represented by a universe encoding the probability distribution over the initial state,
 * and a transition model, which maps a state to a universe encoding the probability distribution
 * over the new state. An implementation of Filtering must implement the advanceTime,
 * computeCurrentDistribution, and computeCurrentExpectation methods.
 * 
 * Querying and asserting evidence to a filtering algorithm are done using references. This is because references are stable over time,
 * while the particular elements they refer to are not.
 * 
 * @param static A static universe that other universes may depend upon.
 * @param initial The initial universe.
 * @param transition A transition function from a universe at the old time step to a new.
 */

abstract class Filtering(static: Universe = new Universe(), initial: Universe, transition: (Universe, Universe) => Universe) extends Algorithm {

  static.registerAlgorithm(this)

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

  /* The following methods will be defined by AnytimeFiltering and OneTimeFiltering */

  /**
   * Returns the distribution over the element referred to by the reference at the current time point.
   */
  def currentDistribution[T](reference: Reference[T]): Stream[(Double, T)]

  /**
   * Returns the expectation of the element referred to by the reference
   * under the given function at the current time point.
   */
  def currentExpectation[T](reference: Reference[T], function: T => Double): Double

  /**
   * Returns the probability that the element referred to by the reference
   * satisfies the given predicate at the current time point.
   */
  def currentProbability[T](reference: Reference[T], predicate: T => Boolean): Double

  /**
   * Returns the probability that the element referred to by the reference
   * produces the given value at the current time point.
   */
  def currentProbability[T](reference: Reference[T], value: T): Double =
    currentProbability(reference, (t: T) => t == value)
}
