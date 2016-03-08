/*
 * NormalKernelDensityEstimator.scala
 * A density estimator using a normal kernel
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 20, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.particlebp

import scala.math._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.algorithm.factored.IntDensityEstimator
import com.cra.figaro.algorithm.factored.DoubleDensityEstimator
import com.cra.figaro.algorithm.factored.DensityException
import com.cra.figaro.algorithm.factored.DensityEstimator

/**
 * A density estimator that uses normal kernel density estimation to estimate the density at an arbitrary point
 * from a set of (probability, value) samples.
 */
trait NormalKernelDensityEstimator extends DensityEstimator with DoubleDensityEstimator with IntDensityEstimator {

  /**
   * Get the bandwidth to use for the normal kernel for this set of samples
   */
  def getBandwidth(samples: List[(Double, Double)]): Double
  
  private def normalizer(bw: Double) = 1.0 / sqrt(2.0 * Pi * bw)

  /*
   * Estimates the density using a normal kernel and the given samples
   */
  def getDensity(pt: Double, samples: List[(Double, Double)]): Double = {
    val bandwidth = getBandwidth(samples)
    val gmmDensities = samples.map { s =>
      val d = Normal.density(0, bandwidth, normalizer(bandwidth))(pt - s._2)
      d * s._1
    }
    gmmDensities.sum
  }
  
  def getDensity(pt: Int, samples: List[(Double, Int)]): Double = {
    getDensity(pt.toDouble, samples.map(s => (s._1, s._2.toDouble)))
  }
  
  def getDensity(pt: Any, samples: List[(Double, Any)]): Double = {
    pt match {
      case d: Double => getDensity(d, samples.asInstanceOf[List[(Double, Double)]])
      case i: Int => getDensity(i, samples.asInstanceOf[List[(Double, Int)]])
      case _ => throw new DensityException("Density estimation not supported for type")
    }
  }

}

