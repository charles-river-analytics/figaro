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

import scala.collection.mutable

/**
 * Saves the mutable state of a universe. This is useful for algorithms that need to maintain a state over elements, but
 * involve a sub-procedure that mutates the state of the universe. This class provides the functionality for restoring
 * the previous state. Immutable.
 *
 * @param universe Universe to save. Information about the current state of this universe is copied in the constructor.
 */
class UniverseState(universe: Universe) {
  // Immutable types
  val myContextStack = universe.myContextStack

  // Mutable types, stored as immutable types
  val myActiveElements = universe.myActiveElements.toSet
  val myStochasticElements = universe.myStochasticElements.toSet

  val myConditionedElements = universe.myConditionedElements.toSet
  val myConstrainedElements = universe.myConstrainedElements.toSet

  val myUses = makeImmutable(universe.myUses)
  val myUsedBy = makeImmutable(universe.myUsedBy)
  val myRecursiveUses = makeImmutable(universe.myRecursiveUses)
  val myRecursiveUsedBy = makeImmutable(universe.myRecursiveUsedBy)

  // States over elements
  val elementStates: Map[Element[_], ElementState] = myActiveElements.map(e => (e, new ElementState(e))).toMap

  /**
   * Restores the universe to its state at the time of construction of this class. In general, this means that any calls
   * to the public API in `Universe` and `Element` will behave as if they were called when this class was instantiated,
   * regardless of what calls to the API were made since instantiation.
   *
   * There are subtle exceptions to this rule. In particular, registered universe maps, element maps, and universe maps
   * will not be changed. Additionally, `myElementMap` from `ElementCollection` will not be changed.
   *
   * Note that classes that extend functionality of `Element` and `Universe` with additional mutability may have
   * undefined behavior with respect to this mutable information. For example, parameters that store learned values will
   * not have their learned values copied.
   *
   * Since this class is immutable, this method can be called multiple times to repeatedly restore the same state after
   * subsequent changes to the universe are made.
   */
  def restore(): Unit = {
    // Deactivate any newly created elements
    for(newElement <- universe.myActiveElements -- myActiveElements) newElement.deactivate()

    // For immutable types, we can just replace the references
    universe.myContextStack = myContextStack

    // For mutable types, we get the reference to the set and replace its contents
    replace(universe.myActiveElements, myActiveElements)
    replace(universe.myStochasticElements, myStochasticElements)
    replace(universe.myConditionedElements, myConditionedElements)
    replace(universe.myConstrainedElements, myConstrainedElements)

    replace(universe.myUses, myUses)
    replace(universe.myUsedBy, myUsedBy)
    replace(universe.myRecursiveUses, myRecursiveUses)
    replace(universe.myRecursiveUsedBy, myRecursiveUsedBy)

    // Element states update on their own
    elementStates.values.foreach(_.restore())
  }

  /**
   * Replace the contents of a mutable set with the contents of an immutable set.
   * @param toReplace Set whose elements should be replaced.
   * @param replaceWith Set of elements to replace with.
   */
  private def replace[T](toReplace: mutable.Set[T], replaceWith: TraversableOnce[T]): Unit = {
    toReplace.clear()
    toReplace ++= replaceWith
  }

  /**
   * Replace the contents of a mutable map to mutable sets with the contents of an immutable map to immutable sets.
   * @param toReplace Map whose elements should be replaced.
   * @param replaceWith Map of elements to replace with.
   */
  private def replace[T](toReplace: mutable.Map[T, mutable.Set[T]], replaceWith: Map[T, Set[T]]): Unit = {
    toReplace.clear()
    for((key, value) <- replaceWith) {
      toReplace(key) = mutable.Set() ++ value
    }
  }

  private def makeImmutable[T](map: mutable.Map[T, mutable.Set[T]]): Map[T, Set[T]] = {
    // map.toMap.mapValues(_.toSet)
    // Wait! Calling mapValues returns a VIEW into the original map, which we don't want because the underlying sets are
    // mutable. See Scala issues SI-4776.
    map.map{ case (key, value) => (key, value.toSet) }.toMap
  }
}

/**
 * Saves the mutable state of a single element. This only saves and restores the mutable information found in the
 * `Element` class; it does not propagate this information to the universe level. Immutable.
 *
 * @param element Element to save. Information about the current state of this element is copied in the constructor.
 */
class ElementState(val element: Element[_]) {
  // Immutable types
  val active = element.active
  val setFlag = element.setFlag

  val value: element.Value = element.value
  val randomness: element.Randomness = element.randomness

  val myPragmas: List[Pragma[element.Value]] = element.myPragmas

  val myConditions = element.myConditions
  val myConstraints = element.myConstraints
  val observation: Option[element.Value] = element.observation

  val myContext = element.myContext

  // Mutable types, stored as immutable types
  val myDirectContextContents = element.directContextContents.toSet

  /**
   * Restores the element to its state at the time of construction of this class.
   *
   * This only restores mutable information found in the `Element` class; any mutable information added by subclasses
   * will be ignored. Furthermore, information about this element is not propagated to the universe level.
   *
   * Since this class is immutable, this method can be called multiple times to repeatedly restore the same state after
   * subsequent changes to the element are made.
   */
  def restore(): Unit = {
    // For immutable types, we can just replace the references
    element.active = active
    element.setFlag = setFlag

    element.value = value
    element.randomness = randomness

    element.myPragmas = myPragmas

    element.myConditions = myConditions
    element.myConstraints = myConstraints
    element.observation = observation

    element.myContext = myContext

    // For mutable types, we get the reference to the set and replace its contents
    val referenceToMyDirectContextContents = element.directContextContents
    referenceToMyDirectContextContents.clear()
    referenceToMyDirectContextContents ++= myDirectContextContents
  }
}
