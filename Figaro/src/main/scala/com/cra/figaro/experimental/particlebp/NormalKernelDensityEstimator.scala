package com.cra.figaro.experimental.particlebp

import scala.math._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.algorithm.factored.IntDensityEstimator
import com.cra.figaro.algorithm.factored.DoubleDensityEstimator
import com.cra.figaro.algorithm.factored.DensityException
import com.cra.figaro.algorithm.factored.DensityEstimator

trait NormalKernelDensityEstimator extends DensityEstimator with DoubleDensityEstimator with IntDensityEstimator {

  def getBandwidth(samples: List[(Double, Double)]): Double
  
  private def normalizer(bw: Double) = 1.0 / sqrt(2.0 * Pi * bw)

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

