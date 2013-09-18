/*
 * Settable.scala
 * Used as a constant in previous universes of filtering
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jun 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.language._

private[filtering] class Settable[T](name: Name[T], var myValue: T, collection: ElementCollection) extends Deterministic[T](name, collection) with Atomic[T] {
  override def set(newValue: T) { myValue = newValue; super.set(newValue) }

  def generateValue() = myValue

  def density(v: T) = if (v == value) 1.0; else 0.0
}