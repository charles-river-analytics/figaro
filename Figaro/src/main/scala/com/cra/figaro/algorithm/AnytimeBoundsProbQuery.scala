/*
 * AnytimeBoundsProbQuery.scala
 * Anytime algorithms that compute bounds on conditional probabilities of query elements.
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
import akka.pattern.ask

/**
 * One-time algorithms that compute bounds on conditional probabilities of query elements. A class that implements this
 * trait must implement initialize, runStep, computeAllProbabilityBounds, and computeExpectationBounds methods.
 */
trait AnytimeBoundsProbQuery extends BoundsProbQueryAlgorithm with AnytimeProbQuery {
  /**
   * A message instructing the handler to compute all probability bounds of the target element.
   */
  case class ComputeAllProbabilityBounds[T](target: Element[T]) extends Service

  /**
   * A message from the handler containing all probability bounds of the previously requested element.
   */
  case class AllProbabilityBounds[T](bounds: Stream[(Double, Double, T)]) extends Response

  /**
   * A message instructing the handler to compute bounds on the expectation of the target element under the given function.
   */
  case class ComputeExpectationBounds[T](target: Element[T], function: T => Double, lower: Option[Double], upper: Option[Double]) extends Service

  /**
   * A message from the handler containing the bounds on the expectation of the previously requested element and function.
   */
  case class ExpectationBounds[T](bounds: (Double, Double)) extends Response

  /**
   * A message instructing the handler to compute bounds on the probability of the predicate for the target element.
   */
  case class ComputeProbabilityBounds[T](target: Element[T], predicate: T => Boolean) extends Service

  /**
   * A message from the handler containing the bounds on the probability of the previously requested element and predicate.
   */
  case class ProbabilityBounds[T](bounds: (Double, Double)) extends Response

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
      case ComputeAllProbabilityBounds(target) =>
        AllProbabilityBounds(computeAllProbabilityBounds(target))
      case ComputeExpectationBounds(target, function, lower, upper) =>
        ExpectationBounds(computeExpectationBounds(target, function, lower, upper))
      case ComputeProbabilityBounds(target, predicate) =>
        ProbabilityBounds(computeProbabilityBounds(target, predicate))
    }

  protected def doAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    awaitResponse(runner ? Handle(ComputeAllProbabilityBounds(target)), messageTimeout.duration) match {
      case AllProbabilityBounds(result) => result.asInstanceOf[Stream[(Double, Double, T)]]
      case _ => Stream()
    }
  }

  protected def doExpectationBounds[T](target: Element[T], function: T => Double, lower: Option[Double], upper: Option[Double]): (Double, Double) = {
    awaitResponse(runner ? Handle(ComputeExpectationBounds(target, function, lower, upper)), messageTimeout.duration) match {
      case ExpectationBounds(bounds) => bounds
      case _ => (Double.NegativeInfinity, Double.PositiveInfinity)
    }
  }

  protected def doProbabilityBounds[T](target: Element[T], predicate: T => Boolean): (Double, Double) = {
    awaitResponse(runner ? Handle(ComputeProbabilityBounds(target, predicate)), messageTimeout.duration) match {
      case ProbabilityBounds(bounds) => bounds
      case _ => (0.0, 1.0)
    }
  }
}
