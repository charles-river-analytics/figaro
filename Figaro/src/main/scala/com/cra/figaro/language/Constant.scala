/*
 * Constant.scala
 * Constant elements.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Elements that always produce the same value.
 */

class Constant[T](name: Name[T], val constant: T, collection: ElementCollection)
  extends Deterministic[T](name, collection) with Atomic[T] with Cacheable[T] {
  def generateValue() = constant

  def density(t: T) = if (t == constant) Double.PositiveInfinity; else 0.0

  override def toString = "Constant(" + constant.toString + ")"
}

object Constant {
  /**
   * Elements that always produce the same value.
   */
  def apply[T](constant: T)(implicit name: Name[T], collection: ElementCollection) =
    new Constant(name, constant, collection)
}
