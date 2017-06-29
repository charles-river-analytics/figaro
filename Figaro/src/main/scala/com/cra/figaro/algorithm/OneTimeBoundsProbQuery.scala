/*
 * OneTimeBoundsProbQuery.scala
 * One-time algorithms that compute bounds on conditional probabilities of query elements.
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
 * One-time algorithms that compute bounds on conditional probabilities of query elements. A class that implements this
 * trait must implement run, computeAllProbabilityBounds, and computeExpectationBounds methods.
 */
trait OneTimeBoundsProbQuery extends BoundsProbQueryAlgorithm with OneTimeProbQuery {
  protected def doAllProbabilityBounds[T](target: Element[T]): Stream[(Double, Double, T)] = {
    computeAllProbabilityBounds(target)
  }

  protected def doExpectationBounds[T](target: Element[T], function: T => Double, lower: Option[Double], upper: Option[Double]): (Double, Double) = {
    computeExpectationBounds(target, function, lower, upper)
  }

  protected def doProbabilityBounds[T](target: Element[T], predicate: T => Boolean): (Double, Double) = {
    computeProbabilityBounds(target, predicate)
  }
}
