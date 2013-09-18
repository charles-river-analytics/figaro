/*
 * Atomic.scala
 * Atomic elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.language

/**
 * The Atomic trait characterizes elements that do not depend on any related elements.
 */

trait Atomic[T] extends Element[T] {
  /**
   * Returns an empty list.
   */
  def args: List[Element[_]] = List()

  /** The probability density of a value. */
  def density(t: T): Double
}