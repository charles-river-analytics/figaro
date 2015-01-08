/*
 * Parameter.scala
 * Elements which can learn their value from observations.
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
 * Trait of learnable parameters. Parameters are elements which can learn their value from data.
 *
 */

/*
abstract trait ElementDouble extends ObjectParameterType
abstract trait ElementArray extends ObjectParameterType
*/


abstract trait Parameter[T] extends Atomic[T] {

  /**
   * Sets the hyperparameters of this parameter to their most likely value based on the sufficient statistics provided.
   */
  def maximize(sufficientStatistics: Seq[Double])
  /**
   * Returns a zero vector of sufficient statistics.
   */
  def zeroSufficientStatistics: Seq[Double]
  /**
   * Returns a sufficient statistics vector corresponding to the evidence that an element using this parameter 
   * took on the value a.
   */
  private[figaro] def sufficientStatistics[A](a: A): Seq[Double]
  /*
   * Returns a sufficient statistics vector with '1' in the position corresponding with the specified index
   */
  private[figaro] def sufficientStatistics[A](a: Int): Seq[Double]

  /**
   * The expected value of the parameter.
   */
  def expectedValue: T
  
  /**
   * The most likely value of the parameter (maximum a posteriori) given the evidence.
   */
  def MAPValue: T
}

trait DoubleParameter extends Parameter[Double] 
trait ArrayParameter extends Parameter[Array[Double]]



