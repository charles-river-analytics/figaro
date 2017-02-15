/*
 * AnytimeMarginalMAP.scala
 * Anytime algorithms that compute the most likely values of some elements, and marginalize over all other elements.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jun 27, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.marginalmap

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import akka.pattern.ask

/**
 * Anytime algorithms that compute most likely values of some elements, and marginalize over all other elements.
 * A class that implements this trait must implement initialize, runStep, and computeMostLikelyValue methods.
 */
trait AnytimeMarginalMAP extends MarginalMAPAlgorithm with Anytime {
  /**
   * A message instructing the handler to compute the most likely value of the target element.
   */
  case class ComputeMostLikelyValue[T](target: Element[T]) extends Service

  /**
   * A message from the handler containing the most likely value of the previously requested element.
   */
  case class MostLikelyValue[T](value: T) extends Response

  def handle(service: Service): Response =
    service match {
      case ComputeMostLikelyValue(target) =>
        MostLikelyValue(computeMostLikelyValue(target))
    }

  protected def doMostLikelyValue[T](target: Element[T]): T = {
    awaitResponse(runner ? Handle(ComputeMostLikelyValue(target)), messageTimeout.duration) match {
      case MostLikelyValue(result) => result.asInstanceOf[T]
      case _ => {
        println("Error: Response not recognized from algorithm")
        target.value
      }
    }
  }
}