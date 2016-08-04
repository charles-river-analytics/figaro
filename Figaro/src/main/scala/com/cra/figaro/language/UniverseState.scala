/*
 * UniverseState.scala
 * Saving and restoring universe state.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 3, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.language

/**
 * Saves the mutable state of a universe. This is useful for algorithms that need to maintain a state over elements, but
 * involve a sub-procedure that mutates the state of the universe. This class provides the functionality for restoring
 * the previous state. Immutable.
 *
 * @param universe Universe to save. Information about the current state of this universe is copied in the constructor.
 */
class UniverseState(universe: Universe) {

  /**
   * Restores the universe to its state at the time of construction of this class. In general, this means that any calls
   * to the public API in `Universe` and `Element` will behave as if they were called when this class was instantiated,
   * regardless of what calls to the API were made since instantiation.
   *
   * There are subtle exceptions to this rule. In particular, registered universe maps, element maps, and universe maps
   * will not be changed.
   *
   * Note that classes that extend functionality of `Element` and `Universe` with additional mutability may have
   * undefined behavior with respect to this mutable information. For example, parameters that store learned values will
   * not have their learned values copied.
   *
   * Since this class is immutable, this method can be called multiple times to repeatedly restore the same state after
   * subsequent changes to the universe are made.
   */
  def restore(): Unit = {

  }
}

/**
 * Saves the mutable state of a single element.
 * @param element
 */
class ElementState(element: Element[_]) {
  val value = element.value
}
