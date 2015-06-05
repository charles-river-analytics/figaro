/*
 * AnytimeMPE.scala
 * Anytime algorithms that compute most likely values of elements.
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
import akka.pattern.{ask}

/**
 * Anytime algorithms that compute most likely values of elements.
 * A class that implements this trait must implement initialize, runStep, computeDistribution,
 * and computeExpectation methods.
 */
trait AnytimeMPE extends MPEAlgorithm with Anytime {
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
        MostLikelyValue(mostLikelyValue(target))
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
