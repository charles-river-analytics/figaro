/*
 * KernelDensity.scala
 * Element representing a kernel density estimate
 * 
 * Created By:      Dan Garant (dgarant@cra.com)
 * Creation Date:   May 27, 2016
 * 
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.atomic.continuous

import com.cra.figaro.language._

/**
 * Performs density estimation using a Gaussian kernel to smooth density estimates around observed inputs.
 * @param samples Observed points
 * @param bandwidth Parameter of the Gaussian kernel  
 */
class KernelDensity(name: Name[Double], val samples: Seq[Double], val bandwidth: Double, collection: ElementCollection) 
  extends Element[Double](name, collection) with Atomic[Double] {
  
  // this represents the Gaussian kernel centered at one of the input points 
  val normalElt = Normal(0, bandwidth)
  
  // randomness is a random index along with a random element from normalElt
  type Randomness = (Int, normalElt.Randomness)
  
  /** Generates a random sample index and offset */
  def generateRandomness:Randomness = {
    val idx = com.cra.figaro.util.random.nextInt(samples.length)
    val rand = normalElt.generateRandomness()
    (idx, rand)
  }
  
  /** Generates a random value from the KD distribution */
  def generateValue(rand:Randomness):Double = {
    val shift = normalElt.generateValue(rand._2) 
    return samples(rand._1) + shift
  }
  
  /** Computes the density of a new  point */
  def density(point:Double):Double = {
    val densities = samples.map(s => {
      normalElt.density(s - point)
    })
    
    densities.sum / densities.length 
  }
  
  override def toString = "KernelDensity(bandwidth=" + this.bandwidth + ")"
}

object KernelDensity {
  /**
   * Create a kernel density estimator with specified bandwidth
   */
  def apply(samples: Seq[Double], bandwidth: Double)(implicit name: Name[Double], collection: ElementCollection) =
    new KernelDensity(name, samples, bandwidth, collection)  
}