/*
 * ValueSet.scala
 * Sets of values that could also include as yet unspecified values.
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
 * A value set that possibly contains unspecified values.
 */
class ValueSet[T](val xvalues: Set[Extended[T]]) {
  /**
   * Add an extended value to this value set.
   * If the extended value is a Star and the value set already contains a Star, we do not add it,
   * since there is no need for more than one Star. 
   */  
  def +(x: Extended[T]): ValueSet[T] = {
    if (!x.isRegular && hasStar) this; else new ValueSet(xvalues + x) 
  }
  
  /**
   * Returns the union of this value set with that value set.
   * If both have a Star, only one Star is kept in the result.
   */
  def ++(that: ValueSet[T]): ValueSet[T] = {
    if (hasStar && that.hasStar) new ValueSet(xvalues ++ that.xvalues.filter(_.isRegular))
    else new ValueSet(xvalues ++ that.xvalues)
  }
  
  /**
   * True if this value set does not contain any unspecified values.
   */
  val hasStar = xvalues.exists(!_.isRegular)
  
  /**
   * The regular values of this value set as plain values.
   */
  val regularValues = xvalues.filter(_.isRegular).map(_.value)

  /*
  /**
   * Returns the particular Star value in this value set. Throws an exception if there is no Star.
   */
  def starValue: Star[T] = {
    try {
      values.find(_.isInstanceOf[Star[T]]).get.asInstanceOf[Star[T]]
    } catch {
      case _: NoSuchElementException => throw new RuntimeException("Attempt to get the Star value of a value set without a Star")
    }
  }
  */
  
  /**
   * Apply a function to each value while keeping the same Star nature.
   */
  def map[U](f: T => U): ValueSet[U] = {
    def xf(x: Extended[T]): Extended[U] = {
      x match {
        case Regular(t) => Regular(f(t))
        case _ => Star[U]
      }
    }
    new ValueSet(xvalues.map(xf(_)))
  }
  
  override def toString = "{" + xvalues.mkString(", ") + "}"
}

object ValueSet {
  def withoutStar[T](values: Set[T]) = {
    val xs: Set[Extended[T]] = values.map(Regular(_))
    new ValueSet(xs)
  }
  
  def withStar[T](values: Set[T]) = {
    val xs: Set[Extended[T]] = values.map(Regular(_))
    new ValueSet(xs + Star[T])
  }
}

