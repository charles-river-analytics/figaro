package com.cra.figaro.experimental.particlebp

/**
 * Class to compute the normal kernel density estimation of a set of samples
 * using Silverman's rule of thumb to automatically compute the bandwidth
 */
class AutomaticDensityEstimator extends NormalKernelDensityEstimator {

  def getBandwidth(samples: List[(Double, Double)]): Double = {
    val mean = (0.0 /: samples)((c: Double, n: (Double, Double)) => c + n._1 * n._2)
    val std = math.sqrt((0.0 /: samples)((c: Double, n: (Double, Double)) => c + (n._1 - mean) * (n._1 - mean) * n._2))
    val bw = 1.06*std*math.pow(samples.size.toDouble, -0.2)
    
    bw
  }
  
}