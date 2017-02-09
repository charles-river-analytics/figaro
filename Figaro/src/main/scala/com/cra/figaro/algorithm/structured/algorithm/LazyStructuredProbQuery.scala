/*
 * LazyStructuredProbQuery.scala
 * Lazy SFI algorithms that compute probability bounds.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jan 09, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm._
import com.cra.figaro.language._

import akka.pattern.ask

trait LazyStructuredProbQuery extends StructuredProbQueryAlgorithm with LazyStructured {
  /**
   * Return an estimate from the marginal probability distribution over the target of the lower and upper bounds,
   * respectively, of the particular value given.
   * Particular algorithms must implement this method.
   */
  def computeProbabilityBounds[T](target: Element[T], value: T): (Double, Double)

  /**
   * Helper method implemented in OneTime and Anytime versions of this algorithm.
   */
  protected def doProbabilityBounds[T](target: Element[T], value: T): (Double, Double)

  /**
   * Return an estimate from the marginal probability distribution over the target of the lower and upper bounds,
   * respectively, of the particular value given.
   * Throws NotATargetException if called on a target that is not in the list of targets of the algorithm.
   * Throws AlgorithmInactiveException if the algorithm is inactive.
   */
  def probabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    check(target)
    doProbabilityBounds(target, value)
  }
}

trait OneTimeLazyStructuredProbQuery extends LazyStructuredProbQuery with OneTimeStructuredProbQuery {
  override protected def doProbabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    computeProbabilityBounds(target, value)
  }
}

trait AnytimeLazyStructuredProbQuery extends LazyStructuredProbQuery with AnytimeStructuredProbQuery {
  /**
   * A message instructing the handler to compute the probability bounds of the target element and value.
   */
  case class ComputeProbabilityBounds[T](target: Element[T], value: T) extends Service

  /**
   * A message from the handler containing the lower and upper bounds of the probability of value queried.
   */
  case class ProbabilityBounds(bounds: (Double, Double)) extends Response

  override def handle(service: Service): Response =
    service match {
      case ComputeDistribution(target) =>
        Distribution(computeDistribution(target))
      case ComputeExpectation(target, function) =>
        Expectation(computeExpectation(target, function))
      case ComputeProbability(target, predicate) =>
        Probability(computeProbability(target, predicate))
      case ComputeProjection(target) =>
        Projection(computeProjection(target))
      case ComputeProbabilityBounds(target, value) =>
        ProbabilityBounds(computeProbabilityBounds(target, value))
    }

  override protected def doProbabilityBounds[T](target: Element[T], value: T): (Double, Double) = {
    awaitResponse(runner ? Handle(ComputeProbabilityBounds(target, value)), messageTimeout.duration) match {
      case ProbabilityBounds(bounds) => bounds
      case _ => (0.0, 1.0)
    }
  }
}
