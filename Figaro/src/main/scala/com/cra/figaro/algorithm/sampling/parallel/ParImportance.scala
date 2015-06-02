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

package com.cra.figaro.algorithm.sampling.parallel

import scala.collection.parallel.ParSeq
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm._
import com.cra.figaro.language._

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
    new ParSampler(algs, targets: _*) with ParAnytime {
      
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
    new ParSampler(algs, targets: _*) with ParOneTime with ProbEvidenceQuery {
      
      override val parAlgs: ParSeq[Importance with OneTimeProbQuerySampler with ProbEvidenceQuery] = algs.par
      
      /**
        * Compute the probability of the given named evidence.
        * Takes the conditions and constraints in the model as part of the model definition.
        * This method takes care of creating and running the necessary algorithms.
        */
      override def probabilityOfEvidence(evidence: List[NamedEvidence[_]]): Double = {
        val poes = parAlgs.toList.map { alg => 
          alg.probabilityOfEvidence(evidence)
        }
        val total = getTotalWeight
        val weightedPOEs = algs zip poes map { case (alg, poe) =>
          // raise from log space and apply to POE
          poe * math.exp(alg.getTotalWeight - total)
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
