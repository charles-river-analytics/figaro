/*
 * DensityEstimator.scala
 * Class for TBD
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 9, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

/**
 * Doc needed
 */
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