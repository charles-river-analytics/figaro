/*
 * AutomaticDensityEstimator.scala
 * Class to compute the normal kernel density estimation of a set of samples
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

/**
 * Class to compute the normal kernel density estimation of a set of samples
 * using Silverman's rule of thumb to automatically compute the bandwidth.
 */
class AutomaticDensityEstimator extends NormalKernelDensityEstimator {

  def getBandwidth(samples: List[(Double, Double)]): Double = {
    val mean = (0.0 /: samples)((c: Double, n: (Double, Double)) => c + n._1 * n._2)
    val std = math.sqrt((0.0 /: samples)((c: Double, n: (Double, Double)) => c + (n._1 - mean) * (n._1 - mean) * n._2))
    val bw = 1.06*std*math.pow(samples.size.toDouble, -0.2)
    
    bw
  }
  
}