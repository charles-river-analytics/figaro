/*
 * ParProbQueryAlgorithm.scala
 * Parallel version of algorithms that compute conditional probabilities of queries.
 * 
 * Created By:      Lee Kellogg (lkellog@cra.com)
 * Creation Date:   May 11, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling

import com.cra.figaro.language.Reference

/**
 * A parallel version of algorithms that compute conditional probabilities of queries. Uses References
 * instead of elements since we are dealing with multiple universes. This provides the functionality
 * of com.cra.figaro.algorithm.ProbQueryAlgorithm while leaving the mechanics of the anytime 
 * or onetime versions of the algorithms to the algorithms themselves.
 */
trait ParProbQueryAlgorithm extends BaseProbQuerySampler[Reference] with ParSamplingAlgorithm {
  
  protected def doDistribution[T](target: Reference[T]) = computeDistribution(target)

  protected def doExpectation[T](target: Reference[T], function: T => Double) = computeExpectation(target, function)

  protected def doProbability[T](target: Reference[T], predicate: T => Boolean) = computeProbability(target, predicate)

  override protected def doProjection[T](target: Reference[T]) = computeProjection(target)
}
