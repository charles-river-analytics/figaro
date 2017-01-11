/*
 * StructuredProbQueryAlgorithm.scala
 * SFI algorithms that compute conditional probabilities of queries.
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   December 30, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors.{BoundsSumProductSemiring, Factor, SumProductSemiring}
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.structured._

abstract class StructuredProbQueryAlgorithm(val universe: Universe, val queryTargets: Element[_]*)
  extends StructuredAlgorithm with ProbQueryAlgorithm {

  override def problemTargets = queryTargets.toList

  // Extracted solutions are joint factors over targets.
  override type ExtractedSolution = Factor[Double]

  override def extractSolution(): ExtractedSolution = {
    problem.solution.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))
  }

  override def processSolutions(extractedSolutions: Map[Bounds, Factor[Double]]): Unit = {
    // TODO what happens if these are sparse?
    val jointLower = if(extractedSolutions.size == 1) extractedSolutions.head._2 else extractedSolutions(Lower)
    val jointUpper = if(extractedSolutions.size == 1) extractedSolutions.head._2 else extractedSolutions(Upper)

    assert(jointLower.variables == jointUpper.variables)
    val allIndicesIndexed = jointLower.getIndices.zipWithIndex.toIndexedSeq

    // TODO can these be anything other than 1.0? Not if we don't do normalization!
    val lowerTotal = jointLower.foldLeft(0.0, _ + _)
    val upperTotal = jointUpper.foldLeft(0.0, _ + _)

    val lowerBounds = for((indices, _) <- allIndicesIndexed) yield jointLower.get(indices) / upperTotal

    val ranges = jointLower.variables.map(_.range.toIndexedSeq)

    val upperBounds = for((indices, i) <- allIndicesIndexed) yield {
      val (consistentIndexed, inconsistentIndexed) = allIndicesIndexed.partition { case (otherIndices, _) =>
        indices.zip(otherIndices).zip(ranges).forall{ case ((index, otherIndex), range) =>
          // Because the range is a set, two extended values are equal if and only if their indices are equal
          index == otherIndex || !range(otherIndex).isRegular
        }
      }

      val consistentUpper = consistentIndexed.map { case (_, j) => lowerBounds(j) }
      val inconsistentLower = inconsistentIndexed.map { case (otherIndices, _) => jointUpper.get(otherIndices) }

      (consistentUpper.sum / lowerTotal) min (1.0 - inconsistentLower.sum)
    }

    val result = jointLower.createFactor(jointLower.parents, jointLower.output, BoundsSumProductSemiring())
    for((indices, i) <- allIndicesIndexed) {
      result.set(indices, (lowerBounds(i), upperBounds(i)))
    }
    // TODO marginalize result
  }

  /**
   * Compute the (normalized) marginalization of the joint factor to the given target element.
   */
  protected def marginalizedTargetFactor[T](target: Element[T], jointFactor: Factor[Double]): Factor[Double] = {
    val targetVar = collection(target).variable
    val unnormalizedTargetFactor = jointFactor.marginalizeTo(targetVar)
    val z = unnormalizedTargetFactor.foldLeft(0.0, _ + _)
    unnormalizedTargetFactor.mapTo((d: Double) => d / z)
  }

  /**
   * Computes the normalized distribution over a single target element.
   * Throws an IllegalArgumentException if the range of the target contains star.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    // TODO is this really correct even if the target range doesn't contain star?
    val targetVar = collection(target).variable
    if(targetVar.valueSet.hasStar) throw new IllegalArgumentException("target range contains *")
    val factor = processedSolutions(Lower)(target)
    val dist = factor.getIndices.map(indices => (factor.get(indices), targetVar.range(indices.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  /**
   * Computes the expectation of a given function for single target element.
   * Throws an IllegalArgumentException if the range of the target contains star.
   */
  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }
}

trait OneTimeStructuredProbQuery extends StructuredProbQueryAlgorithm with OneTimeStructured with OneTimeProbQuery

trait AnytimeStructuredProbQuery extends StructuredProbQueryAlgorithm with AnytimeStructured with AnytimeProbQuery
