/*
 * Parameterized.scala
 * Elements which accept learnable parameters
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

  /**
  * Trait of elements which accept learnable parameters. 
  * Parameterized elements are compound elements whose outcome is determined by a learnable parameter.
  */
trait Parameterized[T] extends Element[T] with HasDensity[T] {
  /**
  * The parameter for this element.
  */
  val parameters: Set[Parameter[_]]
  
  /**
   * Convert a distribution from this element into sufficient statistics for the specified parameter
   */
  def distributionToStatistics(p: Parameter[_], distribution: Stream[(Double, T)]): Seq[Double]
}

trait SingleParameterized[T] extends Parameterized[T] {
  val parameter: Parameter[_]
  override val parameters: Set[Parameter[_]] = Set(parameter)
  /**
   * Convert a distribution from this element into sufficient statistics for the specified parameter
   */
  override def distributionToStatistics(p: Parameter[_], distribution: Stream[(Double, T)]): Seq[Double] = {
    if (p == parameter) {
      distributionToStatistics(distribution)
    }
    else {
      p.zeroSufficientStatistics
    }
    
  }
    /**
   * Convert a distribution from this element into sufficient statistics
   */
  def distributionToStatistics(distribution: Stream[(Double, T)]): Seq[Double]
}
