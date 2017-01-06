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
import com.cra.figaro.algorithm.{AnytimeProbQuery, OneTimeProbQuery, ProbQueryAlgorithm}
import com.cra.figaro.algorithm.factored.factors.{Factor, SumProductSemiring}
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.structured._

abstract class StructuredProbQueryAlgorithm(val universe: Universe, val queryTargets: Element[_]*)
  extends StructuredAlgorithm with ProbQueryAlgorithm {

  override def problemTargets = queryTargets.toList

  // Solutions marginalized to individual targets for lower and upper bounds, respectively.
  private var lowerTargetFactors: Map[Element[_], Factor[Double]] = Map()
  private var upperTargetFactors: Map[Element[_], Factor[Double]] = Map()

  override def recordSolution(bounds: Bounds): Unit = {
    val joint = problem.solution.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))
    val marginalized = queryTargets.map(target => (target, marginalizedTargetFactor(target, joint)))
                                   .toMap[Element[_], Factor[Double]]
    if(bounds == Lower) {
      lowerTargetFactors = marginalized
    }
    else {
      upperTargetFactors = marginalized
    }
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
    val targetVar = collection(target).variable
    if(targetVar.valueSet.hasStar) throw new IllegalArgumentException("target range contains *")
    val factor = lowerTargetFactors(target)
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
