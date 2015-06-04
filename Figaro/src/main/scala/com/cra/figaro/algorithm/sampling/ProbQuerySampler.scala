package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.language._
import com.cra.figaro.algorithm.BaseProbQueryAlgorithm
import scala.language.higherKinds

/**
 * Sampling algorithms that compute conditional probabilities of queries on elements,
 * and that use the projection of all the samples of a target variable to calculate the
 * distribution of that variable or the expectation of a function on that variable.
 */
trait ProbQuerySampler extends BaseProbQuerySampler[Element] {
  val universe: Universe
}

/**
 * A base trait for sampling algorithms that compute conditional probabilities of queries,
 * and that use the projection of all the samples of a target variable to calculate the
 * distribution of that variable or the expectation of a function on that variable.
 * Generic type U is either `Element` or `Reference`.
 */
trait BaseProbQuerySampler[U[_]] extends BaseProbQueryAlgorithm[U] {
  
  /**
   * Total weight of samples taken, in log space
   */
  def getTotalWeight: Double

  /**
   * Return an estimate of the expectation of the function under the marginal probability distribution
   * of the target.
   */
  def computeExpectation[T](target: U[T], function: T => Double) = {
    val contributions = computeProjection(target) map (pair => function(pair._1) * pair._2)
    (0.0 /: contributions)(_ + _)
  }

  /**
   * Return an estimate of the expectation of the function under the marginal probability distribution
   * of the target.
   */
  def computeDistribution[T](target: U[T]): Stream[(Double, T)] =
    computeProjection(target) map (_.swap) toStream
    
}