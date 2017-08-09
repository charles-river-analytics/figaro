/*
 * Deterministic.scala
 * Elements with no randomness.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Elements with no randomness.
 */

abstract class Deterministic[Value](name: Name[Value], collection: ElementCollection)
  extends Element[Value](name, collection) {
  type Randomness = Null

  def generateRandomness() = null

  /**
   * Generate a value with no randomness.
   */
  def generateValue(): Value

  def generateValue(rand: Randomness) = generateValue()
}
