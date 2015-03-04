/*
 * EMTerminationCriteria.scala
 * Class for defining termination of EM algorithms.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Nov 7, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.learning

import com.cra.figaro.language.Parameter

/**
 * Termination criteria for EM algorithms. A termination criteria can be passed as an argument to the EM apply method.
 */
abstract class EMTerminationCriteria {
  type SufficientStatistics = Map[Parameter[_], Seq[Double]]
  def apply(s: SufficientStatistics): Boolean
}

/**
 * Terminate when the maximum number of iterations has been reached
 */
class MaxIterations(val max: Int) extends EMTerminationCriteria {
  var currentIterations = 0
  override def apply(s: SufficientStatistics): Boolean = {
    currentIterations += 1
    if (currentIterations < max) false else true
  }
}

/**
 * Terminate when the magnitude of sufficient statistics does not exhibit a change greater than the specified tolerance.
 */
class SufficientStatisticsMagnitudes(val tolerance: Double) extends EMTerminationCriteria {
  var previousSufficientStatistics = Map.empty[Parameter[_], Seq[Double]]

  def difference(x: Seq[Double], y: Seq[Double]): Double = {
    require(x.size == y.size)
    val sum = (for ((a, b) <- x zip y) yield Math.abs(a - b).toDouble)
    sum.sum / (x.size.toDouble)
  }

  override def apply(s: SufficientStatistics): Boolean = {
    if (previousSufficientStatistics.isEmpty) {
      previousSufficientStatistics = s
      return false
    }

    val delta = for (k <- s.keys) yield {
      difference(s(k), previousSufficientStatistics(k))
    }
    val totalDelta = delta.sum / (delta.size.toDouble)
    previousSufficientStatistics = s
    if (totalDelta < tolerance) {
      return true
    }
    return false
  }
}

object EMTerminationCriteria {
  /**
   * Terminate when the maximum number of iterations has been reached
   */
  def maxIterations(max: Int): () => EMTerminationCriteria = () => new MaxIterations(max)
  /**
   * Terminate when the magnitude of sufficient statistics does not exhibit a change greater than the specified tolerance.
   */
  def sufficientStatisticsMagnitude(tolerance: Double): () => EMTerminationCriteria = () => new SufficientStatisticsMagnitudes(tolerance)
}