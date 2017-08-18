/*
 * BoundsProbQueryAlgorithm.scala
 * Algorithms that compute bounds on conditional probabilities of queries.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 23, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm

import com.cra.figaro.language._

/**
 * Algorithms that compute bounds on conditional probabilities of queries over elements in a universe. The regular
 * ProbQuery methods are also available, but these methods may throw an exception if the algorithm cannot produce an
 * exact answer.
 */
trait BoundsProbQueryAlgorithm extends ProbQueryAlgorithm {
  /**
   * Return an estimate of the marginal probability distribution over the target that lists each value with its
   * probability bounds. Each entry is a triple (lower, upper, value). The result is a lazy stream. It is up to the
   * algorithm how the stream is ordered.
   */
  def computeAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)]

  /**
   * Return an estimate of the bounds on the expectation of the function under the marginal probability distribution
   * of the target. The function is assumed to be bounded between the specified lower and upper bounds, if provided.
   * Otherwise, the lower and upper bounds of the function using the current known values of the target are used.
   */
  def computeExpectationBounds[T](target: Element[T], function: T => Double, bounds: Option[(Double, Double)]): (Double, Double)

  /**
   * Return an estimate of the probability of the bounds on the predicate under the marginal probability distribution
   * of the target.
   */
  def computeProbabilityBounds[T](target: Element[T], predicate: T => Boolean): (Double, Double) = {
    computeExpectationBounds(target, (t: T) => if(predicate(t)) 1.0 else 0.0, Some((0.0, 1.0)))
  }


  /*
   * The following methods are defined in either the onetime or anytime versions of this class,
   * and do not need to be defined by particular algorithm implementations.
   */

  protected def doAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)]

  protected def doExpectationBounds[T](target: Element[T], function: T => Double, bounds: Option[(Double, Double)]): (Double, Double)

  protected def doProbabilityBounds[T](target: Element[T], predicate: (T) => Boolean): (Double, Double)

  /**
   * Return an estimate of the marginal probability distribution over the target that lists each value with its
   * probability bounds. Each entry is a triple (lower, upper, value). The result is a lazy stream. It is up to the
   * algorithm how the stream is ordered.
   * @param target Element for which to compute bounds.
   * @throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * @throws AlgorithmInactiveException if the algorithm is inactive.
   * @return Bounds on the probability of each value for this element.
   */
  def allProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    check(target)
    doAllProbabilityBounds(target)
  }

  /**
   * Return an estimate of the bounds on the expectation of the function under the marginal probability distribution
   * of the target. The function is assumed to be bounded between the specified lower and upper bounds.
   * @param target Element for which to compute bounds.
   * @param function Function whose expectation is computed.
   * @param lower Lower bound on the function.
   * @param upper Upper bound on the function.
   * @throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * @throws AlgorithmInactiveException if the algorithm is inactive.
   * @throws IllegalArgumentException if the bounds given on the function are tighter than the actual bounds on the
   * function, using the current known values of the target.
   * @return Bounds on the expectation of this function for this element.
   */
  def expectationBounds[T](target: Element[T], function: T => Double, lower: Double, upper: Double): (Double, Double) = {
    check(target)
    doExpectationBounds(target, function, Some((lower, upper)))
  }

  /**
   * Return an estimate of the bounds on the expectation of the function under the marginal probability distribution
   * of the target. The function is assumed to be bounded according to the currently known values of the target. Thus,
   * one should generally only use this when the range of the target is finite and known beforehand. Otherwise, one can
   * use the overloaded version of this method that specifies explicit bounds on the function.
   * @param target Element for which to compute bounds.
   * @param function Function whose expectation is computed.
   * @throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * @throws AlgorithmInactiveException if the algorithm is inactive.
   * @return Bounds on the expectation of this function for this element.
   */
  def expectationBounds[T](target: Element[T], function: T => Double): (Double, Double) = {
    check(target)
    doExpectationBounds(target, function, None)
  }

  /**
   * Return an estimate of the probability of the bounds on the predicate under the marginal probability distribution
   * of the target.
   * @param target Element for which to compute bounds.
   * @param predicate Function whose probability of evaluating to true is computed.
   * @throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * @throws AlgorithmInactiveException if the algorithm is inactive.
   * @return Bounds on the probability of this function for this element.
   */
  def probabilityBounds[T](target: Element[T], predicate: T => Boolean): (Double, Double) = {
    check(target)
    doProbabilityBounds(target, predicate)
  }

  def probabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    check(target)
    doProbabilityBounds(target, (t: T) => t == value)
  }
}
