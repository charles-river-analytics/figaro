/*
 * ProbQueryAlgorithm.scala
 * Algorithms that compute conditional probabilities of queries.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._

/**
 * Algorithms that compute conditional probabilities of queries.
 * 
 */
trait ProbQueryAlgorithm
  extends Algorithm {
  val universe: Universe
  /*
   * @param targets List of elements that can be queried after running the algorithm.
   */
  val queryTargets: Seq[Element[_]]
  /*
   * Particular implementations of algorithm must provide the following two methods.
   */

  /**
   * Return an estimate of the marginal probability distribution over the target that lists each element
   * with its probability. The result is a lazy stream. It is up to the algorithm how the stream is
   * ordered.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)]

  /**
   * Return an estimate of the expectation of the function under the marginal probability distribution
   * of the target.
   */
  def computeExpectation[T](target: Element[T], function: T => Double): Double

  /**
   * Return an estimate of the probability of the predicate under the marginal probability distribution
   * of the target.
   */
  def computeProbability[T](target: Element[T], predicate: T => Boolean): Double = {
    computeExpectation(target, (t: T) => if (predicate(t)) 1.0; else 0.0)
  }
    

  /*
   * The following methods are defined in either the onetime or anytime versions of this class, 
   * and do not need to be defined by particular algorithm implementations.
   */

  protected def doDistribution[T](target: Element[T]): Stream[(Double, T)]

  protected def doExpectation[T](target: Element[T], function: T => Double): Double

  protected def doProbability[T](target: Element[T], predicate: T => Boolean): Double

  private def check[T](target: Element[T]): Unit = {
    if (!active) throw new AlgorithmInactiveException
    if (!(queryTargets contains target)) throw new NotATargetException(target)
  }

  /**
   * Return an estimate of the marginal probability distribution over the target that lists each element
   * with its probability. The result is a lazy stream. It is up to the algorithm how the stream is
   * ordered.
   * Throws NotATargetException if called on a target that is not in the list of
   * targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def distribution[T](target: Element[T]): Stream[(Double, T)] = {
    check(target)
    doDistribution(target)
  }

  /**
   * Return an estimate of the expectation of the function under the marginal probability distribution
   * of the target.
   * Throws NotATargetException if called on a target that is not in the list of
   * targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def expectation[T](target: Element[T], function: T => Double): Double = {
    check(target)
    doExpectation(target, function)
  }

  /**
   * Return the mean of the probability density function for the given continuous element.
   */
  def mean(target: Element[Double]): Double = {
    expectation(target, (d: Double) => d)
  }

  /**
   * Return the variance of the probability density function for the given continuous element.
   */
  def variance(target: Element[Double]): Double = {
    val m = mean(target)
    val ex2 = expectation(target, (d: Double) => d * d)
    ex2 - m*m
  }
  
  /**
   * Return an element representing the posterior probability distribution of the given element.
   */
  def posteriorElement[T](target: Element[T], universe: Universe = Universe.universe): Element[T] = {
    Select(distribution(target).toList:_*)("", universe)
  }
  
  /**
   * Return an estimate of the probability of the predicate under the marginal probability distribution
   * of the target.
   * Throws NotATargetException if called on a target that is not in the list of
   * targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    check(target)
    doProbability(target, predicate)
  }

  /**
   * Return an estimate of the probability that the target produces the value.
   * Throws NotATargetException if called on a target that is not in the list of
   * targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def probability[T](target: Element[T], value: T): Double = {
    check(target)
    doProbability(target, (t: T) => t == value)
  }

  universe.registerAlgorithm(this)
}
