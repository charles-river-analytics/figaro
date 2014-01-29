/*
 * Extended.scala
 * Extended values, which could also be Star (unspecified).
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 27, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
   * Is this value consistent with that value? Regular values are consistent if they are equal, while Star is consistent with everything.
   */
  //def consistent(that: Extended[T]): Boolean
  
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
  /**
   * A regular value is consistent with the same regular value or with Star.
   */
  /*
   * 
   def consistent(that: Extended[T]): Boolean = {
    that match {
      case Star() => true
      case Regular(value1) => value1 == value
    }
  }
*/
  
  def isRegular = true
  
}

/**
 * The special value Star, which stands for as yet unspecified values.
 * When computing a lower bound to probabilities, we can assume that Star will eventually
 * evaluate to something other than what is needed to make the query have a particular value.
 */
case class Star[T]() extends Extended[T] {
  /**
   * Star is consistent with everything.
   */
  /*
  def consistent(that: Extended[T]): Boolean = true
  */
  //val id = Star.genSym()
  
  def isRegular = false
  
  def value: T = throw new IllegalArgumentException("Attempt to get value of Star")
  
  override def toString = "*"
}
/*
object Star {
  private var count = 0
  
  def genSym() {
    count += 1
    count
  }
}
*/