/*
 * Evidence.scala
 * Evidence, and association of evidence with references.
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

import java.lang.IllegalArgumentException

/**
 * Evidence that can be associated with an element.
 */
sealed abstract class Evidence[T] {
  /** The type over which this evidence is defined. Any element on which this evidence is asserted must also be over this type. */
  type EvidenceType = T
  
  /** Assert this evidence on the given element. The second argument is an optional contingency. */
  def set(element: Element[T], contingency: Element.Contingency = List()): Unit
}

/**
 * Evidence representing a condition on an element.
 * 
 * @param predicate The predicate that must be satisfied by the element.
 */
case class Condition[T](predicate: T => Boolean) extends Evidence[T] {
  /** Assert this evidence on the given element. The second argument is an optional contingency. */
  def set(element: Element[T], contingency: Element.Contingency = List()): Unit =
    element.setCondition(predicate, contingency)
}

/**
 * Evidence representing a constraint on an element.
 * 
 * @param function The constraint to be applied to the element.
 */
case class Constraint[T](function: T => Double) extends Evidence[T] {
  /** Assert this evidence on the given element. The second argument is an optional contingency. */  
  def set(element: Element[T], contingency: Element.Contingency = List()): Unit =
    element.setConstraint(function, contingency)
}

/**
 * Evidence representing a log constraint on an element.
 * 
 * @param function The constraint (in log form) to be applied to the element.
 */
case class LogConstraint[T](function: T => Double) extends Evidence[T] {
  /** Assert this evidence on the given element. The second argument is an optional contingency. */
  def set(element: Element[T], contingency: Element.Contingency = List()): Unit =
    element.setLogConstraint(function, contingency)
}

/**
 * Evidence representing observing a particular value for the element.
 * 
 * @param value The observed value of the element.
 */
case class Observation[T](value: T) extends Evidence[T] {
  /** Assert this evidence on the given element. The second argument is an optional contingency. */
  def set(element: Element[T], contingency: Element.Contingency = List()): Unit =
    element.setCondition((t: T) => t == value, contingency)
}

/**
 * Association of evidence with a reference.
 * 
 * @param reference The reference to apply the evidence.
 * @param contingency Optional contingency that must be satisfied for this evidence to be applied.
 */
case class NamedEvidence[T](reference: Reference[T], evidence: Evidence[T], contingency: Element.Contingency = List())

class InvalidEvidenceException extends IllegalArgumentException
