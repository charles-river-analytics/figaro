package com.cra.figaro.algorithm.sampling

import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.language._
import com.cra.figaro.algorithm.BaseProbQueryAlgorithm
import scala.language.higherKinds

trait ProbQuerySampler extends BaseProbQuerySampler[Element] {
  val universe: Universe
}

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