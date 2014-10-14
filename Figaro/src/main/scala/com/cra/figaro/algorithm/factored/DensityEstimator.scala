package com.cra.figaro.algorithm.factored

class DensityException(s: String) extends RuntimeException(s)

abstract class DensityEstimator {
  def getDensity(pt: Any, samples: List[(Double, Any)]): Double  
}

trait DoubleDensityEstimator extends DensityEstimator {
  def getDensity(pt: Double, samples: List[(Double, Double)]): Double
}

trait IntDensityEstimator extends DensityEstimator {
  def getDensity(pt: Int, samples: List[(Double, Int)]): Double  
}

class ConstantDensityEstimator extends DensityEstimator {
  def getDensity(pt: Any, samples: List[(Double, Any)]): Double = 1.0  
}