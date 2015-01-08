/*
 * Universe.scala
 * Universes of elements.
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

import com.cra.figaro.algorithm._
import com.cra.figaro.util._
import collection.immutable.Stack
import scala.annotation.tailrec
import scala.collection.mutable.{ Set, Map }
import scala.language.{ implicitConversions, existentials }
import java.lang.IllegalArgumentException
import scala.collection.generic.Shrinkable

/**
 * A universe is a collection of elements that can be used by a reasoning algorithm.
 *
 * Ordinarily, the arguments of elements in a universe must also belong to the universe. You can
 * optionally supply a list of parent elements that contains other elements that can be arguments.
 *
 * @param parentElements The parent elements on which this universe depends.
 */
class Universe(val parentElements: List[Element[_]] = List()) extends ElementCollection {

  /** The universe to which elements in this universe belongs, which is, of course, this universe.
   */
  override val universe = this

  private val myActiveElements: Set[Element[_]] = Set()

  /**
   * The active elements in the universe.
   */
  def activeElements: List[Element[_]] = myActiveElements.toList

  /** Elements in the universe that are not defined in the context of another element. */
  def permanentElements: List[Element[_]] = myActiveElements.toList filterNot (_.isTemporary)

  private val myConditionedElements: Set[Element[_]] = Set()

  /** Elements in the universe that have had a condition applied to them. */
  def conditionedElements: List[Element[_]] = myConditionedElements.toList

  private[language] def makeConditioned(elem: Element[_]) { myConditionedElements += elem }

  private[language] def makeUnconditioned(elem: Element[_]) { myConditionedElements -= elem }

  private val myConstrainedElements: Set[Element[_]] = Set()

  /** Elements in the universe that have had a constraint applied to them. */
  def constrainedElements: List[Element[_]] = myConstrainedElements.toList

  private[language] def makeConstrained(elem: Element[_]) { myConstrainedElements += elem }

  private[language] def makeUnconstrained(elem: Element[_]) { myConstrainedElements -= elem }

  private val myStochasticElements = new HashSelectableSet[Element[_]]

  /**
   * The active non-deterministic elements in the universe.
   */
  def stochasticElements: List[Element[_]] = myStochasticElements.toList

  /**
   * Selects a non-deterministic element uniformly at random.
   */
  def randomStochasticElement(): Element[_] = myStochasticElements.select()

  /* A Chain can create new Elements. For memory management purposes, we may want to release such an Element when
   * the argument to Chain takes on a different value so that the Element is no longer relevant to avoid memory leaks.
   * This raises a problem, because many algorithms create maps and caches using those elements which then need to be
   * cleared of the Element being released. To solve this problem, we introduce the notion of a context stack. When
   * a Chain is entered, it pushes its context onto the stack, and when it is left the context is popped. When an
   * Element is created and activated, we make a note of its context, and also add it to the set of Elements that
   * are contained in the context. This makes it possible to delete all the Elements defined within a certain
   * Chain from a Map.
   *
   * A special case is that if the context of an Element is empty, it must be a permanent element created outside of
   * any Chain. Conversely, if it is not empty, it must have been created within a chain, so it is temporary. It is
   * possible to remove all temporary Elements from a Map.
   */
  private var myContextStack: List[Element[_]] = List()

  private[figaro] def contextStack = myContextStack

  private[figaro] def context(element: Element[_]): List[Element[_]] = element.context

  private[figaro] def inContext(dependent: Element[_], container: Element[_]): Boolean = dependent.context contains container

  /*
   * Maps an Element to all the Elements that were created directly in its context.
   * This defines a directed graph of elements. The complete set of Elements created in a context is computed
   * by calling reachable on this graph.
   */
  private[figaro] def contextContents(element: Element[_]): List[Element[_]] =
    reachable(element, (e: Element[_]) => e.directContextContents).toList

  private[figaro] def pushContext(element: Element[_]): Unit = {
    myContextStack ::= element
  }

  /*
   * popContext pops all elements at and above the given element from the context stack, if it contains the given
   * element, otherwise it does nothing.
   */
  private[figaro] def popContext(element: Element[_]): Unit = {
    if (myContextStack contains element)
      myContextStack = myContextStack dropWhile (_ != element) drop 1
  }

  private val myUses: Map[Element[_], Set[Element[_]]] = Map()

  private val myUsedBy: Map[Element[_], Set[Element[_]]] = Map()

  private val myRecursiveUsedBy: Map[Element[_], Set[Element[_]]] = Map()
  private val myRecursiveUses: Map[Element[_], Set[Element[_]]] = Map()

  /**
   * Returns the set of elements that the given element uses in its generation, either directly or recursively.
   */
  def uses(elem: Element[_]): Set[Element[_]] = {
    elemGraphBuilder(List[(Element[_], Set[Element[_]])]() :+ (elem, Set[Element[_]]()), myUses, myRecursiveUses)
    myRecursiveUses.getOrElse(elem, Set())
  }

  /**
   * Returns the set of elements that use the given element in their generation, either directly or
   * recursively.
   */
  def usedBy(elem: Element[_]): Set[Element[_]] = {
    elemGraphBuilder(List[(Element[_], Set[Element[_]])]() :+ (elem, Set[Element[_]]()), myUsedBy, myRecursiveUsedBy)
    myRecursiveUsedBy.getOrElse(elem, Set())
  }

  /**
   * Returns the set of elements that are directly used by the given element, without recursing.
   */
  def directlyUsedBy(elem: Element[_]): Set[Element[_]] = myUsedBy.getOrElse(elem, Set())

  private[figaro] def registerUses[T, U](user: Element[T], used: Element[U]): Unit = {
    if (!(myUses contains user)) myUses += user -> Set()
    if (!(myUsedBy contains used)) myUsedBy += used -> Set()
    if (used.universe == this && !(myUsedBy(used) contains user)) {
      myUses(user) += used
      myUsedBy(used) += user
      myRecursiveUsedBy.clear
      myRecursiveUses.clear
    }
  }

  private[figaro] def deregisterUses[T, U](user: Element[T], used: Element[U]): Unit = {
    if(used.universe == this){
      if (myUses.contains(user)) myUses(user) -= used
      if (myUsedBy.contains(used)) myUsedBy(used) -= user
      myRecursiveUsedBy.clear
      myRecursiveUses.clear
    }
  }

  private[language] def activate(element: Element[_]): Unit = {
    if (element.active)
      throw new IllegalArgumentException("Activating active element")
//    if (element.args exists (!_.active))
//      throw new IllegalArgumentException("Attempting to activate element with inactive argument")
    element.args.filter(!_.active).foreach(activate(_))
    myActiveElements.add(element)
    if (!element.isInstanceOf[Deterministic[_]]) myStochasticElements.add(element)

    if (myContextStack.nonEmpty) {
      element.myContext = myContextStack
      myContextStack.head.addContextContents(element)
    }
    element.args foreach (registerUses(element, _))
    element.active = true
//    myRecursiveUsedBy.clear
//    myRecursiveUses.clear
  }

  private[language] def deactivate(element: Element[_]): Unit = {
    if (!element.active)
      throw new IllegalArgumentException("Deactivating inactive element")
    // When we deactivate an element, we must deactivate all elements that were created in its context
    deactivate(element.directContextContents.clone)
    // We must pop the context stack if we are deactivating the top element.
    popContext(element)

    if (element.isTemporary) {
      element.context.head.removeContextContents(element) //
      deregisterUses(element.context.head, element)
    }
    element.active = false
    element.args foreach (deregisterUses(element, _))

    //Make sure that if another element uses this element, that we remove it from the UsedBy
    if (myUsedBy.contains(element) && !myUsedBy(element).isEmpty) {
      myUsedBy(element) foreach (deregisterUses(_, element))
    }
    myUsedBy -= element
    myUses -= element
    myRecursiveUsedBy -= element
    myRecursiveUses -= element
    if (!element.isInstanceOf[Deterministic[_]]) myStochasticElements.remove(element)
    myActiveElements.remove(element)
    element.collection.remove(element)
    registeredMaps foreach (_ -= element)

  }

  /**
   * Safely deactivate all the given elements.
   */
  def deactivate(elems: Traversable[Element[_]]): Unit = {
    // We need to check if elements are still active because they might be deactivated when other elements are
    // deactivated.
    elems foreach (elem => if (elem.active) deactivate(elem))
  }

  /**
   * Clear the universe of all elements.
   */
  def clear(): Unit = {
    // We need to make a copy of myActiveElements because it gets clobbered by deactivate.
    // Context stack should be automatically emptied.
    deactivate(myActiveElements.toList)
    myContextStack = List()
    registeredUniverseMaps foreach (_ -= this)
    registeredAlgorithms foreach (a => if (a.isActive) a.kill())
    registeredAlgorithms.clear()
  }

  /**
   * Clear the universe of all temporary elements. I.e., elements that were created as the result
   * of expanding a chain.
   */
  def clearTemporaries(): Unit = {
    permanentElements foreach (_.clearContext)
    myContextStack = List()
  }

  /**
   * Clear the universe of all elements with no explicit name. This is useful, e.g., in dynamic models, where only named elements can influence subsequent time steps.
   */
  def clearUnnamed(): Unit = {
    deactivate(myActiveElements.toList filter (_.name.isEmpty))
  }

  private[figaro] def clearContext[T](element: Element[T]): Unit =
    deactivate(contextContents(element))

  private var registeredMaps: Set[Shrinkable[Element[_]]] = Set()

  private var registeredUniverseMaps: Set[Map[Universe, _]] = Set()

  private var registeredAlgorithms: Set[Algorithm] = Set()

  /**
   * Register a map so that elements are removed from it when they are deactivated.
   * This avoids memory management problems.
   */
  def register(collection: Shrinkable[Element[_]]): Unit = registeredMaps += collection

  /** Deregister a map of elements. */
  def deregister(collection: Shrinkable[Element[_]]): Unit = registeredMaps -= collection

  // Immediately register the constrained and conditioned elements
  register(myConditionedElements)
  register(myConstrainedElements)

  /**
   * Register the maps that this universe is used as a key.
   * Needed to make sure Universe is garbage collected when cleared and dereferenced.
   */
  def registerUniverse(map: Map[Universe, _]): Unit = registeredUniverseMaps += map

  /** Deregister a map that uses this universe as a key. */
  def deregisterUniverse(map: Map[Universe, _]): Unit = registeredUniverseMaps -= map

  /**
   * Register algorithms that use this universe.
   * When the Universe is cleared, all previous algorithms are no longer valid,
   * so they must be killed (if still running).
   */
  def registerAlgorithm(alg: Algorithm): Unit = registeredAlgorithms += alg

  /** Deregister an algorithm. */
  def deregisterAlgorithm(alg: Algorithm): Unit = registeredAlgorithms -= alg

  /**
   * Returns the elements in the given set that are independent of all other elements in the set.
   * I.e., they do not use any of the elements in the set in their generation. Also returns the
   * remaining non-independent elements.
   */
  def independentElements(elems: Traversable[Element[_]]): (Traversable[Element[_]], Traversable[Element[_]]) =
    elems partition ((e1: Element[_]) => !(elems exists ((e2: Element[_]) => usedBy(e2) contains e1)))

  /**
   * Returns a list of layers of elements in the given set of elements, where the elements in each layer can be
   * generated independently of each other given elements in previous layers.
   */
  def layers(elems: Traversable[Element[_]]): List[List[Element[_]]] = {
    if (elems.isEmpty) List()
    else {
      val (first, rest) = independentElements(elems)
      if (first == Set()) throw new IllegalArgumentException("Cyclic set of elements - cannot produce layers")
      first.toList :: layers(rest)
    }
  }

  override def finalize = {
    clear()
    super.finalize()
  }

  private type Graph = Map[Element[_], Set[Element[_]]]

  /*
   * Recursively (depth-first) builds a usedBy/uses graph and will re-use previously cached paths
   * base is a data structure that just contains the direct edges. existing contains
   * any previous recursive paths that have already been cached.
   */
  @tailrec
  private def elemGraphBuilder(elems: List[(Element[_], Set[Element[_]])], base: Graph, curr: Graph): Graph = {

    if (elems.isEmpty) return curr
    val (elem, visitStack) = elems.head

    if (visitStack.contains(elem)) throw new IllegalArgumentException("Cyclic set of elements - cannot walk graph")
    val toAdd = if (curr.contains(elem)) {
      visitStack.foreach(e => curr.getOrElseUpdate(e, Set()) ++= (curr(elem) + elem))
      List[(Element[_], Set[Element[_]])]()
    } else {
      visitStack.foreach(e => curr.getOrElseUpdate(e, Set()) += elem)
      base.getOrElse(elem, Set()).toList.map(e => (e, (visitStack + elem)))
    }
    return elemGraphBuilder(toAdd ++ elems.tail, base, curr)
  }

}

object Universe {
  /**
   * The current universe.
   * Most of the time, you don't need to specify the universe and can use the current universe as
   * default.
   */
  implicit var universe: Universe = new Universe

  /**
   * Create a new universe and set it to the default.
   */
  def createNew(): Universe = {
    universe = new Universe
    universe
  }
}

//This could go in Evidence instead of here.
object AssertEvidence {

    /**
   * Assert the given evidence associated with references to elements in the collection.
   */
  def apply(evidencePairs: Seq[NamedEvidence[_]]): Unit = {
    for { NamedEvidence(reference, evidence, contingency) <- evidencePairs } Universe.universe.assertEvidence(reference, evidence, contingency)
  }

  def apply[T](evidence: NamedEvidence[T]): Unit = {
    apply(evidence.reference, evidence.evidence)
  }

  /**
   *   Assert the given evidence on the given reference. The third argument is an optional contingency.
   *   This method makes sure to assert the evidence on all possible resolutions of the reference.
   */
  def apply[T](reference: Reference[T], evidence: Evidence[T], contingency: Element.Contingency = List()): Unit = {
    Universe.universe.assertEvidence(reference,evidence,contingency)
  }

}
