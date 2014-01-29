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
class ValueSet[T](val values: Set[Extended[T]]) {
  /**
   * Add an extended value to this value set.
   * If the extended value is a Star and the value set already contains a Star, we do not add it,
   * since there is no need for more than one Star. 
   */  
  def +(x: Extended[T]): ValueSet[T] = {
    if (!x.isRegular && hasStar) this; else new ValueSet(values + x) 
  }
  
  /**
   * Returns the union of this value set with that value set.
   * If both have a Star, only one Star is kept in the result
   */
  def ++(that: ValueSet[T]): ValueSet[T] = {
    if (hasStar && that.hasStar) new ValueSet(values ++ that.values.filter(_.isRegular))
    else new ValueSet(values ++ that.values)
  }
  
  /**
   * Returns true if this value set does not contain any unspecified values.
   */
  def hasStar: Boolean = {
    values.exists(!_.isRegular)
  }

  /**
   * Returns the regular values of this value set as plain values.
   */
  def regularValues: Set[T] = {
    values.filter(_.isRegular).map(_.value)
  }
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
    new ValueSet(values.map(xf(_)))
  }
  
  override def toString = "{" + values.mkString(", ") + "}"
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

/*
/**
 * A value set with no unspecified values.
 */
case class WithoutStar[T](vs: Set[T]) extends ValueSet[T](vs) {
  /**
   * Add an extended value to this value set. If the extended value is Star, the result will have unspecified values.
   */
  def +(x: Extended[T]): ValueSet[T] = {
    x match {
      case Regular(value) => WithoutStar(values + value)
      case s: Star() => WithStar(values)
    }
  }
   
  /** 
   * Returns the union of this value set with that value set. If that value set has unspecified values, so does the result.
   */
  def ++(vs: ValueSet[T]): ValueSet[T] = {
    vs match {
      case WithoutStar(values2) => WithoutStar(values ++ values2)
      case WithStar(values2) => WithStar(values ++ values2)
    }
  } 
  
  val complete = true
  
  def map[U](f: T => U): ValueSet[U] = WithoutStar(vs.map(f))

  /**
   * Convert this value set to a list of extended values that does not include Star.
   */
  def toExtendedList: List[Extended[T]] = vs.map(Regular(_)).toList
}

/**
 * A value set with unspecified values.
 */
case class WithStar[T](s: Star[T], vs: Set[T]) extends ValueSet[T](vs) {
  /**
   * Add an extended value to this value set. The result will always have unspecified values.
   */
  def +(x: Extended[T]): ValueSet[T] = {
    x match {
      case Regular(value) => WithStar(values + value)
      case Star() => WithStar.this
    }
  }

  /** 
   * Returns the union of this value set with that value set. The result always has unspecified values.
   */
  def ++(vs: ValueSet[T]): ValueSet[T] = {
    vs match {
      case WithoutStar(values2) => WithStar(values ++ values2)
      case WithStar(values2) => WithStar(values ++ values2)
    }
  } 
  
  val complete = false

  def map[U](f: T => U): ValueSet[U] = WithStar(vs.map(f))

  /**
   * Convert this value set to a list of extended values that includes Star.
   */
  def toExtendedList: List[Extended[T]] = Star[T]() :: vs.map(Regular(_)).toList
}

*/