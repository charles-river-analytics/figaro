/*
 * ParImportance.scala
 * Parallel importance sampling.
 * 
 * Created By:      Lee Kellogg (lkellog@cra.com)
 * Creation Date:   May 11, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.parallel

import scala.collection.parallel.ParSeq
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm._
import com.cra.figaro.language._

/**
 * Parallel version of Importance sampling. Has a parallel collection of algorithm instances 
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
abstract class ParImportance(algs: Seq[Importance], targets: Reference[_]*) 
extends ParProbQueryAlgorithm {
  
  /** The query targets are references in this case **/
  override val queryTargets: Seq[Reference[_]] = targets.toSeq
  
  /** A parallel collection of algorithms **/
  protected val parAlgs: ParSeq[Importance] = algs.par
  
  /** Get an element from a reference **/
  protected def element[T](alg: Importance, element: Reference[T]): Element[T] = {
    alg.universe.getElementByReference(element)
  }
  
  protected def totalWeight(algs: Seq[Importance]) = (algs.toList map (_.getTotalWeight)).sum

  private def computeCombinedProjection[T](target: Reference[T]): List[(T, Double)] = {
    val algs = parAlgs.toList
    val combinedTotalWeight = totalWeight(algs)
    val flatWeightedProjections: List[(T, Double)] = algs flatMap { alg =>
      val projection: List[(T, Double)] = alg.projection(element(alg, target))
      val algWeight = alg.getTotalWeight / combinedTotalWeight
      projection map { case (v, w) => (v, w * algWeight) }
    }
    val groupedByValue = flatWeightedProjections groupBy (_._1)
    val combinedWeights = groupedByValue mapValues { (group: List[(T, Double)]) => 
      group.map(_._2).sum
    }
    combinedWeights.toList
  }

  /**
   * Return an estimate of the marginal probability distribution over the target that lists each element
   * reference with its probability. The result is a lazy stream. It is up to the algorithm how the stream is
   * ordered.
   */
  def computeDistribution[T](target: Reference[T]): Stream[(Double, T)] = {
    val projection = computeCombinedProjection(target)
    val swapped = projection map (_.swap)
    swapped.toStream
  }

  /**
   * Return an estimate of the expectation of the function under the marginal probability distribution
   * of the target.
   */
  def computeExpectation[T](target: Reference[T], function: T => Double): Double = {
    val projection = computeCombinedProjection(target)
    val contributions = projection map { case (v, w) => function(v) * w }
    contributions.sum
  }
}

object ParImportance {
  
  /**
   * Create a parallel anytime importance sampler with the given target query references.
   * 
   * @param generator a function that returns a universe, with any evidence applied
   * @param numThreads the number of threads to spawn
   * @param targets references to the target elements
   */
  def apply(generator: () => Universe, numThreads: Int, targets: Reference[_]*) = {
    val algs = for ( _ <- 1 to numThreads) yield {
      val universe = generator()
      val elements = targets.map(universe.getElementByReference(_))
      Importance(elements: _*)(universe)
    }
    new ParImportance(algs, targets: _*) with ParAnytime {
      
      override val parAlgs: ParSeq[Importance with AnytimeProbQuerySampler] = algs.par
      
    }
  }

  /**
   * Create a parallel one-time importance sampler with the given target query references
   * using the given number of samples.
   * 
   * @param generator a function that returns a universe, with any evidence applied
   * @param numThreads the number of threads to spawn
   * @param numSamples the number of samples to take, total, across the threads
   * @param targets references to the target elements
   */
  def apply(generator: () => Universe, numThreads: Int, numSamples: Int, targets: Reference[_]*) = {
    val algs = for ( _ <- 1 to numThreads) yield {
      val universe = generator()
      val elements = targets.map(universe.getElementByReference(_))
      Importance(numSamples / numThreads, elements: _*)(universe)
    }
    new ParImportance(algs, targets: _*) with ParOneTime with ProbEvidenceQuery {
      
      override val parAlgs: ParSeq[Importance with OneTimeProbQuerySampler with ProbEvidenceQuery] = algs.par
      
      /**
        * Compute the probability of the given named evidence.
        * Takes the conditions and constraints in the model as part of the model definition.
        * This method takes care of creating and running the necessary algorithms.
        */
      override def probabilityOfEvidence(evidence: List[NamedEvidence[_]]): Double = {
        val poes = parAlgs.map { alg => 
          alg.probabilityOfEvidence(evidence)
        }
        val total = totalWeight(algs)
        val weightedPOEs = algs zip poes map { case (alg, poe) =>
          poe * (alg.getTotalWeight / total)
        }
        weightedPOEs.sum
      }
    }
  }

  /**
   * Use parallel IS to compute the probability that the given reference element satisfies the given predicate.
   */
  def probability[T](generator: () => Universe, numThreads: Int, target: Reference[T], predicate: T => Boolean): Double = {
    val alg = ParImportance(generator, numThreads, 10000, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use parallel IS to compute the probability that the given reference element has the given value.
   */
  def probability[T](generator: () => Universe, numThreads: Int, target: Reference[T], value: T): Double =
    probability(generator, numThreads, target, (t: T) => t == value)
}
