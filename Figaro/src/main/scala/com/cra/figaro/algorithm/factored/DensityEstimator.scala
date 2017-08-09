/*
 * DensityEstimator.scala
 * Class to estimate densities of marginal distributions for resampling
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 9, 2014
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored


class DensityException(s: String) extends RuntimeException(s)

/**
 * The density estimator class is an abstract class to estimate the density of a point
 * in some space given a list of weighted samples on that space. This class is principally
 * used in Particle BP, where new samples for BP are generated from the estimate of the posterior.
 * The density estimator is used in PBP to estimate densities during a Metropolis-Hastings sampler
 */
abstract class DensityEstimator {
  /**
   * Get's the density of a point from a list of weighted samples
   */
  def getDensity(pt: Any, samples: List[(Double, Any)]): Double  
}

/**
 * Trait for Density estimators on the space of doubles
 */
trait DoubleDensityEstimator extends DensityEstimator {
  def getDensity(pt: Double, samples: List[(Double, Double)]): Double
}

/**
 * Trait for Density estimators on the space of ints
 */
trait IntDensityEstimator extends DensityEstimator {
  def getDensity(pt: Int, samples: List[(Double, Int)]): Double  
}

/**
 * A constant density estimator that always returns 1.0
 */
class ConstantDensityEstimator extends DensityEstimator {
  def getDensity(pt: Any, samples: List[(Double, Any)]): Double = 1.0  
}

