package com.cra.figaro.algorithm.learning

import com.cra.figaro.language.Parameter

abstract class EMTerminationCriteria(val alg: ExpectationMaximization) {
  type SufficientStatistics = Map[Parameter[_], Seq[Double]]
  def apply(s: SufficientStatistics): Boolean
}

class LikelihoodTermination(val tolerance: Double, alg: ExpectationMaximization) extends EMTerminationCriteria(alg) {
  var previousLikelihood = Double.NegativeInfinity
  override def apply(s: SufficientStatistics): Boolean = {
    false
  }
}

class MaxIterations(val iterations: Int, alg: ExpectationMaximization) extends EMTerminationCriteria(alg) {
  var currentIterations = 0
  override def apply(s: SufficientStatistics): Boolean = {
    currentIterations += 1
    if (currentIterations < iterations) false else true
  }
}

class SufficientStatisticsMagnitudes(val tolerance: Double,  alg: ExpectationMaximization) extends EMTerminationCriteria(alg) {
  var previousSufficientStatistics = Map.empty[Parameter[_], Seq[Double]]
  
  def difference(x: Seq[Double], y: Seq[Double]): Double = {
   require(x.size == y.size)
   val sum = (for ((a, b) <- x zip y) yield Math.abs(a - b).toDouble)
   sum.sum/(x.size.toDouble)
  }
  
  override def apply(s: SufficientStatistics): Boolean = {
    if (previousSufficientStatistics.isEmpty) {
      previousSufficientStatistics = s
      return false
    }

    val delta = for (k <- s.keys) yield {
      difference(s(k),previousSufficientStatistics(k))
    }
    val totalDelta = delta.sum/(delta.size.toDouble)
    previousSufficientStatistics = s
    if (totalDelta < tolerance) {
      return true
    }
    return false
  }
}

class BICTermination(val tolerance: Double, alg: ExpectationMaximization) extends EMTerminationCriteria(alg) {
  override def apply(s: SufficientStatistics): Boolean = {
    false
  }
}