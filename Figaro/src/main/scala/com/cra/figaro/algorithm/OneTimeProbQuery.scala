/*
 * OneTimeProbQuery.scala
 * One-time algorithms that compute conditional probability of query elements.
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
 * One-time algorithms that compute conditional probability of query elements.
 * A class that implements this trait must implement run, computeDistribution,
 * and computeExpectation methods.
 */
trait OneTimeProbQuery extends ProbQueryAlgorithm with OneTime {
  protected def doDistribution[T](target: Element[T]): Stream[(Double, T)] = computeDistribution(target)

  protected def doExpectation[T](target: Element[T], function: T => Double): Double =
    computeExpectation(target, function)

  protected def doProbability[T](target: Element[T], predicate: T => Boolean): Double = {
    computeProbability(target, predicate)
  }
  
  override protected def doProjection[T](target: Element[T]): List[(T, Double)] = computeProjection(target)
    
}
