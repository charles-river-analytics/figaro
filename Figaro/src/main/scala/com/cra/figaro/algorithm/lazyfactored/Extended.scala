/*
 * Extended.scala
 * Extended values, which could also be Star (unspecified).
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 27, 2013
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.lazyfactored

/**
 * An extended value, which could either be a regular value or the special value Star.
 */
sealed abstract class Extended[T] {
  /**
   * Return true if the value is a regular value.
   */
  def isRegular: Boolean
  
  /**
   * Return the underlying value. Throws an IllegalArgumentException when given Star.
   */
  def value: T
}

/**
 * A regular value.
 */
case class Regular[T](val value: T) extends Extended[T] {
  def isRegular = true
}

/**
 * The special value Star, which stands for the unknown result of an unexpanded computation.
 * When computing a lower bound to probabilities, we can assume that Star will eventually
 * evaluate to something other than what is needed to make the query have a particular value.
 * Star is a case class so that when we have different variables over the same type that both
 * have value Star, we can say that their values are equal.
 */
case class Star[T]() extends Extended[T] {
  def isRegular = false
  
  def value: T = throw new IllegalArgumentException("Attempt to get value of Star")
  
  override def toString = "*"
}
