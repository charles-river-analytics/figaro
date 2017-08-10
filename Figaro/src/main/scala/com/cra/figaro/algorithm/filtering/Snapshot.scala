/*
 * Snapshot.scala
 * Stores a snapshot of a universe for a particle
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 1, 2012
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.language._
import scala.collection.mutable.Map

/** A record of the values of a set of named elements at a particular point in time. */
class Snapshot {
  /** The values of the elements in the snapshot. */
  private[filtering] val values: Map[Name[_], Any] = Map()

  private[filtering] var universe: Universe = _

  /** Store the values of all named elements in the universe in this snapshot. */
  def store(universe: Universe) {
    this.universe = universe

    values.clear()
    for {
      element <- universe.activeElements
      if !element.name.isEmpty
    } {
      values += element.name -> element.value
    }
  }

  /** Returns the value of the given element in the snapshot, based on the name of the element. */
  def apply[T](element: Element[T]): T = values(element.name).asInstanceOf[T]

  /** Get the value of the element associated with the given reference. */
  def get[T](reference: Reference[T]): T = apply(universe.getElementByReference(reference))

  /** Restore the values stored in the snapshot into the given new universe. */
  def restore(newUniverse: Universe) {
    for {
      (name, value) <- values
    } {
      val element = newUniverse.getElementByReference(name)
      if (!element.active) element.activate
      element.set(value.asInstanceOf[element.Value])
    }
  }
}
