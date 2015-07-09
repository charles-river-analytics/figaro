/*
 * ParSampler.scala
 * Parallel version of a sampling algorithm.
 * 
 * Created By:      Lee Kellogg (lkellogg@cra.com)
 * Creation Date:   Jun 2, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.sampling.parallel

import com.cra.figaro.language._
import com.cra.figaro.util
import com.cra.figaro.algorithm.sampling.BaseProbQuerySampler
import com.cra.figaro.algorithm.sampling.ProbQuerySampler
import scala.collection.parallel.ParSeq

/**
 * Parallel version of a sampling algorithm. Has a parallel collection of algorithm instances 
 * that will do its work on separate threads, over separate universes. Uses Scala's parallel 
 * collections to divide up the work, so it will work best if told to run on a number of threads 
 * less than or equal to the number of worker threads Scala creates when operating over parallel 
 * collections. 
 * 
 * This creates two major differences with how a user interacts with the algorithm. First of all, 
 * rather than defining a model on a universe and then starting the algorithm, the user must 
 * define a function that generates a universe and applies any evidence, and then pass that to 
 * the companion object to create the algorithm. The second major difference is that elements 
 * must be referred to using references, since each variable will exist as multiple elements 
 * across the different universes.
 * 
 * One-time sampling will be faster, since it divides the work over the different threads. 
 * Anytime sampling should provide more accurate results, since it can take more samples over 
 * the same amount of time. Both cases will most likely require more memory, at least using the 
 * current implementation of WeightedSampler, which keeps track of all values and weights that 
 * have been sampled. When querying, a weighted combination of the results of the various threads 
 * is returned. This last step adds some overhead, which should be negligible as long as you are 
 * taking a large number of samples.
 */
abstract class ParSampler(algs: Seq[ProbQuerySampler], targets: Reference[_]*) 
 extends BaseProbQuerySampler[Reference] with ParSamplingAlgorithm {
  
  /** The query targets are references in this case **/
  override val queryTargets: Seq[Reference[_]] = targets.toSeq
  
  /** A parallel collection of algorithms **/
  protected val parAlgs: ParSeq[ProbQuerySampler] = algs.par
  
  /** Get an element from a reference **/
  protected def element[T](alg: ProbQuerySampler, element: Reference[T]): Element[T] = {
    alg.universe.getElementByReference(element)
  }
  
  /** Log sum of total weights of individual algorithms **/
  def getTotalWeight: Double = util.logSumMany(parAlgs.toList map (_.getTotalWeight))

  override protected[algorithm] def computeProjection[T](target: Reference[T]): List[(T, Double)] = {
    val algs = parAlgs.toList
    val combinedTotalWeight = getTotalWeight
    val flatWeightedProjections: List[(T, Double)] = algs flatMap { alg =>
      // raise weights out of log space
      val algWeight = math.exp(alg.getTotalWeight - combinedTotalWeight)

      // projection is already raised from log space
      val projection: List[(T, Double)] = alg.computeProjection(element(alg, target))
      
      // apply weight
      projection map { case (v, w) => (v, w * algWeight) }
    }
    val groupedByValue = flatWeightedProjections groupBy (_._1)
    val combinedWeights = groupedByValue mapValues { (group: List[(T, Double)]) => 
      group.map(_._2).sum
    }
    combinedWeights.toList
  }
  
  /** Methods from BaseProbQueryAlgorithm **/
  protected def doDistribution[T](target: Reference[T]) = computeDistribution(target)
  protected def doExpectation[T](target: Reference[T], function: T => Double) = computeExpectation(target, function)
  protected def doProbability[T](target: Reference[T], predicate: T => Boolean) = computeProbability(target, predicate)
  override protected def doProjection[T](target: Reference[T]) = computeProjection(target)
}
