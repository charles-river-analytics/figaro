/*
 * CPDCase.scala
 * Cases in conditional probability distributions.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 17, 2011
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

/**
 * A case in a conditional probability distribution.
 */
sealed abstract class CPDCase[-T] {
  /**
   * Set of values included in this case.
   */
  def contains(elem: T): Boolean
}

/**
 * A case in a conditional probability distribution that includes the given elements.
 */
case class OneOf[T](elems: T*) extends CPDCase[T] {
  /**
   * Set of values included in this case.
   */
  def contains(elem: T): Boolean = elems contains elem
}

/**
 * A case in a conditional probability distribution that excludes the given elements.
 */
case class NoneOf[T](elems: T*) extends CPDCase[T] {
  /**
   * Set of values included in this case.
   */
  def contains(elem: T): Boolean = !(elems contains elem)
}

/**
 * A "wild-card" case in a conditional probability distribution that includes all elements.
 */
case object * extends CPDCase[Any] {
  /**
   * Set of values included in this case.
   */
  def contains(elem: Any): Boolean = true
}



