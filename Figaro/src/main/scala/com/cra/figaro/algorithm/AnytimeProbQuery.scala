/*
 * AnytimeProbQuery.scala
 * Anytime algorithms that compute conditional probability of query elements.
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
import java.util.concurrent.TimeUnit
import akka.util.Timeout
import akka.pattern.{ask}
import scala.concurrent.duration
import scala.concurrent.Await

/**
 * Anytime algorithms that compute conditional probability of query elements.
 * A class that implements this trait must implement initialize, runStep, computeDistribution,
 * and computeExpectation methods.
 */
trait AnytimeProbQuery extends ProbQueryAlgorithm with Anytime {
 /**
   * A message instructing the handler to compute the distribution of the target element.
   */
  case class ComputeDistribution[T](target: Element[T]) extends Service
 /**
   * A message from the handler containing the distribution of the previously requested element.
   */
  case class Distribution[T](distribution: Stream[(Double, T)]) extends Response
   /**
   * A message instructing the handler to compute the expectation of the target element under the given function.
   */
  case class ComputeExpectation[T](target: Element[T], function: T => Double) extends Service
    /**
   * A message from the handler containing the expected value of the previously requested element and function.
   */
  case class Expectation(expectation: Double) extends Response
   /**
   * A message instructing the handler to compute the probability of the predicate for the target element.
   */
  case class ComputeProbability[T](target: Element[T], predicate: T => Boolean) extends Service
    /**
   * A message from the handler containing the probability of the previously requested predicate and element.
   */
  case class Probability(probability: Double) extends Response

  def handle(service: Service): Response =
    service match {
      case ComputeDistribution(target) =>
        Distribution(computeDistribution(target))
      case ComputeExpectation(target, function) =>
        Expectation(computeExpectation(target, function))
      case ComputeProbability(target, predicate) =>
        Probability(computeProbability(target, predicate))
    }

  implicit val timeout = Timeout(5000, TimeUnit.MILLISECONDS)
  protected def doDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val response = runner ? Handle(ComputeDistribution(target))
    Await.result(response, timeout.duration ).asInstanceOf[Response] match {
      case Distribution(result) => result.asInstanceOf[Stream[(Double, T)]]
      case ExceptionResponse(msg) =>
        println(msg)
        Stream()
      case _ => Stream()
    }
  }

  protected def doExpectation[T](target: Element[T], function: T => Double): Double = {
    val response = runner ? Handle(ComputeExpectation(target, function))
    Await.result(response, timeout.duration ).asInstanceOf[Response] match {
      case Expectation(result) => result
      case ExceptionResponse(msg) =>
        println(msg)
        0.0
      case _ => 0.0
    }
  }

  protected override def doProbability[T](target: Element[T], predicate: T => Boolean): Double = {
    val response = runner ? Handle(ComputeProbability(target, predicate))
    Await.result(response, timeout.duration ).asInstanceOf[Response] match {
      case Probability(result) => result
      case ExceptionResponse(msg) =>
        println(msg)
        0.0
      case _ => 0.0
    }
  }
}
