/*
 * ElementCollection.scala
 * Element collections.
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

import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.lazyfactored.{ValueSet, LazyValues}
import com.cra.figaro.util._
import scala.collection.mutable.Map

/**
 * An element collection contains elements. It can be used to find the elements in it by reference.
 */
trait ElementCollection {
  /**
   * The universe this element collection is part of. Defaults to the current universe at the time the ElementCollection is created.
   * Override this if you want a different universe.
   */
  val universe: Universe = Universe.universe
  
  /* Elements with the same name may belong to the same collection. The most recently added on is used to resolve references. (That is why a List is used to hold the elements
   * with a given name, rather than a Set, so we always know which is the most recent.) When evidence is applied to a name, all the elements with that name have the evidence
   * applied to them.
   */
  private val myElementMap: Map[Name[_], List[Element[_]]] = Map()

  /** All named elements in this collection. */
  @deprecated("Use namedElements instead", "2.3.0.0")
  def allElements: List[Element[_]] = myElementMap.values.flatten.toList
  
  /** All named elements in this collection. */
  def namedElements: List[Element[_]] = myElementMap.values.flatten.toList

  /**
   * Returns a reference element representing the single-valued reference.
   */
  def get[T](reference: Reference[T]): SingleValuedReferenceElement[T] =
    new SingleValuedReferenceElement(this, reference)

  /**
   * Returns a reference element representing the aggregate applied to the multi-valued reference.
   */
  def getAggregate[T, U](aggregate: MultiSet[T] => U)(reference: Reference[T]): Aggregate[T, U] =
    new Aggregate(this, reference, aggregate)

  /**
   * Add an element to the element collection.
   */
  def add[T](element: Element[T]) {
    if (element.name.string != "") {
      val current = myElementMap.getOrElse(element.name, List())
      myElementMap += element.name -> (element :: current)
      // We need to propagate previously created evidence into the new element
      for { RefEv(reference, evidence, contingency) <- collectedEvidence.values } {
        reference match {
          case Name(first) =>
            if (element.name.string == first) evidence.asInstanceOf[Evidence[T]].set(element, contingency)
          case Indirect(first, rest) =>
            if (element.name == first) {
              for {
                value <- Values(universe)(element)
                restEC <- ElementCollection.makeElementCollectionSet(value)
              } {
                restEC.assertEvidence(rest, evidence, Element.ElemVal(element, value) :: contingency)
              }
            }
        }
      }
    }
  }

  /**
   * Remove an element from the element collection.
   */
  def remove[T](element: Element[T]) {
    if (element.name.string != "") {
      val current = myElementMap.getOrElse(element.name, List())
      myElementMap += element.name -> (current.filterNot(_ == element))
    }
  }

  /**
   * Assert the given evidence associated with references to elements in the collection.
   */
  def assertEvidence(evidencePairs: Seq[NamedEvidence[_]]): Unit =
    for { NamedEvidence(reference, evidence, contingency) <- evidencePairs } assertEvidence(reference, evidence, contingency)

  /**
   *   Assert the given evidence on the given reference. The third argument is an optional contingency.
   *   This method makes sure to assert the evidence on all possible resolutions of the reference.
   */

  def assertEvidence[T](reference: Reference[T], evidence: Evidence[T], contingency: Element.Contingency = List()): Unit = {

    val resolutions = allResolutions(reference)
    for { (element, subContingency) <- resolutions } {
      if (element.isDefined) evidence.set(element.get, contingency ::: subContingency)
    }

    // after asserting the evidence, we also have to assert the values of the elements that make this reference resolvable
    val toAssert = findResolvableValues(resolutions)
    toAssert.foreach(e =>
      e._1.asInstanceOf[Element[e._1.Value]].addCondition((v: e._1.Value) => e._2.asInstanceOf[Set[e._1.Value]].contains(v)))
    propagateCollectedEvidence(reference, Some(evidence), contingency)
  }

  /**
   *  Remove any evidence on the given reference. The second argument is an optional contingency.
   *  This method makes sure to remove evidence from all possible resolutions of the reference.
   *  Note: this method removes all conditions and constraints, no matter when they were added.
   */

  def removeEvidence[T](reference: Reference[T], contingency: Element.Contingency = List()): Unit = {
    val resolutions = allResolutions(reference)
    for { (element, subContingency) <- resolutions } {
      if (element.isDefined) {
        element.get.removeConditions(contingency ::: subContingency)
        element.get.removeConstraints(contingency ::: subContingency)
      }
    }
    // have to remove the assertions that made this reference resolvable
    findResolvableValues(resolutions).foreach(e => e._1.asInstanceOf[Element[e._1.Value]].removeConditions())
    propagateCollectedEvidence(reference, None, contingency)
  }

  private def propagateCollectedEvidence[T](reference: Reference[T], evidence: Option[Evidence[T]], contingency: Element.Contingency): Unit = {
    evidence match {
      case Some(e) => collectedEvidence += reference -> RefEv(reference, e, contingency)
      case None => collectedEvidence -= reference
    }
    reference match {
      case Name(n) => ()
      case Indirect(head, tail) =>
        val tailECs =
          for {
            element <- myElementMap.getOrElse(head, List())
            value <- Values(universe)(element)
            ec <- ElementCollection.makeElementCollectionSet(value)
          } {
            ec.propagateCollectedEvidence(tail, evidence, Element.ElemVal(element.asInstanceOf[Element[element.Value]], value.asInstanceOf[element.Value]) :: contingency)
          }
    }
  }

  /*
   * Evidence in the element collection needs to be asserted not only on existing elements but also on future created elements that have the right reference.
   * Therefore, we keep track of 
   */
  private case class RefEv[T](ref: Reference[T], ev: Evidence[T], contingency: Element.Contingency)
  private var collectedEvidence: Map[Reference[_], RefEv[_]] = Map()

  /**
   * Returns all resolutions of the given reference. Considers all possible values of each of the elements on the path and all elements with each name on the path.
   * Along with each possible resolution, it returns the contingency (values of elements along the path) required to make that resolution be the one.
   */
  def allPossibleResolutions[T](reference: Reference[T]): Set[(Element[T], Element.Contingency)] = {
    val all = allResolutions(reference)
    all.filter(p => p._1.isDefined).map(e => (e._1.get, e._2))
  }

  /*
   * Finds all of the valid values of elements needed to resolve a reference
   */
  private def findResolvableValues[T](resolutions: Set[(Option[Element[T]], Element.Contingency)]): Map[Element[_], Set[Any]] = {
    val resolveMap: Map[Element[_], Set[Any]] = Map()
    resolutions.foreach { r =>
      val (possibility, contingency) = r
      contingency.foreach { c =>
        val current = resolveMap.getOrElseUpdate(c.elem, Set())
        if (possibility.isDefined) resolveMap += (c.elem -> (current + c.value))
      }
    }
    resolveMap
  }

  /*
   * Returns all possible valid AND non-valid resolutions of a reference. If a reference is not valid, it
   * places None in the element field of the return set at the lowest level reference that was resolvable
   */
  private def allResolutions[T](reference: Reference[T]): Set[(Option[Element[T]], Element.Contingency)] = {
    reference match {
      case Name(name) =>
        val elems = myElementMap.getOrElse(name, List())
        if (elems.nonEmpty)
          Set((Some(elems.head.asInstanceOf[Element[T]]), List())) // use most recent element with the name
        else
          Set((None, List()))
      case Indirect(head, tail) =>
        val headCheck = myElementMap.getOrElse(head, List()) // use most recent element with the name        
        if (headCheck.nonEmpty) {
          val headElem = myElementMap(head).head // use most recent element with the name
          for {
            headValue: Any <- Values(universe)(headElem)
            headEC: ElementCollection <- ElementCollection.makeElementCollectionSet(headValue) //makeECs(headValue)
            (possibility, contingency) <- headEC.allResolutions(tail)
          } yield {
            possibility match {
              case Some(e) => (possibility, Element.ElemVal(headElem.asInstanceOf[Element[headElem.Value]], headValue.asInstanceOf[headElem.Value]) :: contingency)
              case None => (None, List(Element.ElemVal(headElem.asInstanceOf[Element[headElem.Value]], headValue.asInstanceOf[headElem.Value])))
            }
          }
        } else {
          Set((None, List()))
        }
    }
  }

  /**
   *   Returns true if the reference is resolvable on this collection. This will always use the most
   *   recent element with the reference name.
   */
  def hasRef[T](reference: Reference[T]): Boolean = {
    reference match {
      case Name(name) => myElementMap.getOrElse(name, List()).nonEmpty
      case Indirect(head, tail) =>
        val headCheck = myElementMap.getOrElse(head, List())
        if (headCheck.isEmpty) return false
        val tailHasRef = Values(universe)(headCheck.head).flatMap { e =>
          ElementCollection.makeElementCollectionSet(e).map(_.hasRef(tail))
        }
        tailHasRef.exists(_ == true)
    }
  }

  // The arguments of a reference include the arguments of all its possibilities as well as all references along the
  // way, because the value depends on these references. It also includes all the possibilities since the value of
  // the reference is dependent on their value.
  private[language] def makeArgs(reference: Reference[_]): Set[Element[_]] = {
    reference match {
      case Name(name) =>
        val elems = myElementMap(name)
        elems.head.args.toSet + elems.head // use most recent element with the name
      case Indirect(head, tail) =>
        val headElems = myElementMap(head)
        val headElem = headElems.head // use most recent element with the name
        val restArgs = for {
          headValue <- Values(universe)(headElem)
          headCollection <- ElementCollection.makeElementCollectionSet(headValue)
          arg <- headCollection.makeArgs(tail)
        } yield arg
        restArgs + headElem
    }
  }

  /**
   * Gets the first element in the chain contained by the reference, together with an optional remaining
   * reference. If the reference is simply a name, the element is the referred to element and the remainder is None.
   */
  def getFirst[T](reference: Reference[T]): (Element[_], Option[Reference[T]]) =
    reference match {
      case Name(name) =>
        val elems = myElementMap(name)
        (elems.head, None) // use most recent element with the name
      case Indirect(head, tail) =>
        val headElems = myElementMap(head)
        (headElems.head, Some(tail)) // use most recent element with the name
    }

  /*
   * Follow the single-valued reference in the current state to get the element referred to.
   * Need to clarify: This gets the element *currently* referred to by the reference. Provide example.
   * 
   */
  def getElementByReference[T](reference: Reference[T]): Element[T] =
    reference match {
      case Name(name) =>
        try {
          val elems = myElementMap(name)
          elems.head.asInstanceOf[Element[T]] // use most recent element with the name
        } catch {
          case e: ClassCastException =>
            throw new IllegalArgumentException("Reference has wrong type")
        }
      case Indirect(head, tail) =>
        try {
          val headElems = myElementMap(head)
          headElems.head.generate()
          headElems.head.asInstanceOf[Element[ElementCollection]].value.getElementByReference(tail) // use most recent element with the name
        } catch {
          case e: ClassCastException =>
            throw new IllegalArgumentException("Invalid reference: " + head +
              " does not refer to an element collection")
        }
    }

  /*
   * Follow the multi-valued reference in the current state to get the elements referred to.
   */
  private[figaro] def getManyElementsByReference[T](reference: Reference[T]): Set[Element[T]] =
    reference match {
      case Name(name) =>
        try {
          val elems = myElementMap(name)
          Set(elems.head.asInstanceOf[Element[T]]) // use most recent element with the name 
        } catch {
          case e: ClassCastException =>
            throw new IllegalArgumentException("Reference has wrong type")
        }
      case Indirect(head, tail) =>
        try {
          val headElems = myElementMap(head)
          headElems.head.generate()
          val nextECs = ElementCollection.makeElementCollectionSet(headElems.head.value) // use most recent element with the name
          (Set[Element[T]]() /: nextECs)(_ union _.getManyElementsByReference(tail))
        } catch {
          case e: ClassCastException =>
            throw new IllegalArgumentException("Invalid reference: " + head +
              " does not refer to an element collection")
        }
    }
}

object ElementCollection {
  /**
   * The default element collection is the current universe.
   */
  implicit def default: ElementCollection = Universe.universe

  /* 
   * Turns either a single element collection or a traversable of element collections into an element collection set 
   */
  private[language] def makeElementCollectionSet(value: Any): Set[ElementCollection] = {
    def makeEC[T](s: Set[T]) = s map (_.asInstanceOf[ElementCollection])
    value match {
      case ec: ElementCollection => Set(ec)
      case t: Traversable[_] => makeEC(t.toSet)
      case _ => throw new IllegalArgumentException("Not collections: " + value)
    }
  }
}
