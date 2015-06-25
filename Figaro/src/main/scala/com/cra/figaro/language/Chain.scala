/*
 * Chain.scala
 * Element Chains
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
import scala.collection.mutable.Map
import scala.collection.mutable.Set

/**
 * A Chain(parent, fcn) represents the process that first generates a value for the parent, then
 * applies fcn to get a new Element, and finally generates a value from that new Element.
 *
 * Chain is the common base class for caching and non-caching chains. There is no functional difference
 * between caching and non-caching chains. Algorithms can use the distinction to implement a caching procedure.
 *
 * @param parent The parent element of the chain
 */

class Chain[T, U](name: Name[U], val parent: Element[T], fcn: T => Element[U], collection: ElementCollection)
  extends Deterministic[U](name, collection) {

  def args: List[Element[_]] = List(parent)

  protected def cpd = fcn

  private[figaro] val chainFunction = fcn

  /**
   * The type of the parent argument.
   */
  type ParentType = T

  def generateValue() = {
    if (parent.value == null) parent.generate()
    val resultElement = get(parent.value)
    if (resultElement.value == null) resultElement.generate()
    resultElement.value
  }

  /**
   * Get the distribution over the result corresponding to the given parent value.
   */
  def get(parentValue: T): Element[U] = {
    val result = getResult(parentValue)
    universe.registerUses(this, result)
    result
  }

  private[figaro] def getUncached(parentValue: T): Element[U] = get(parentValue)

  // All elements created in cpd will be created in this Chain's context with a subContext of parentValue
  private def getResult(parentValue: T): Element[U] = {
    universe.pushContext(this)
    val result = cpd(parentValue)
    universe.popContext(this)
    result
  }
  
  override def toString = "Chain(" + parent + ", " + cpd + ")"
}

/**
 * A NonCachingChain is an implementation of Chain with a single element cache.
 */
class NonCachingChain[T, U](name: Name[U], parent: Element[T], cpd: T => Element[U], collection: ElementCollection)
  extends Chain(name, parent, cpd, collection)

/**
 * A CachingChain is an implementation of Chain with a 1000 element cache.
 */
class CachingChain[T, U](name: Name[U], parent: Element[T], cpd: T => Element[U], collection: ElementCollection)
  extends Chain(name, parent, cpd, collection)

object NonCachingChain {
  /** Create a NonCaching chain of 1 argument. */
  def apply[T, U](parent: Element[T], cpd: T => Element[U])(implicit name: Name[U], collection: ElementCollection): NonCachingChain[T, U] =
    new NonCachingChain(name, parent, cpd, collection)

  /** Create a NonCaching chain of 2 arguments. This is implemented as a chain on the single argument consisting of the pair of the two arguments. */
  def apply[T1, T2, U](parent1: Element[T1], parent2: Element[T2], cpd: (T1, T2) => Element[U])(implicit name: Name[U], collection: ElementCollection): NonCachingChain[(T1, T2), U] =
    new NonCachingChain(
      name,
      new Apply2("", parent1, parent2, (t1: T1, t2: T2) => (t1, t2), collection),
      (pair: (T1, T2)) => cpd(pair._1, pair._2),
      collection)
}

object Chain {
  /**
   * Create a chain of 1 argument.
   * The Chain factory constructor chooses either a CachingChain or NonCachingChain depending on the cacheability of the parent.
   * The parent's cacheability is determined by the parent.isCachable field. This is normally determined by the type of the element.
   */
  def apply[T, U](parent: Element[T], cpd: T => Element[U])(implicit name: Name[U], collection: ElementCollection): Chain[T, U] = {
    if (parent.isCachable) new CachingChain(name, parent, cpd, collection)
    else new NonCachingChain(name, parent, cpd, collection)
  }

  /**
   * Create a chain of 2 arguments. This is implemented as a chain on the single argument consisting of the pair of the two arguments.
   * If both arguments are cachable, a CachingChain will be produced, otherwise a NonCaching chain.
   */
  def apply[T1, T2, U](parent1: Element[T1], parent2: Element[T2], cpd: (T1, T2) => Element[U])(implicit name: Name[U], collection: ElementCollection): Chain[(T1, T2), U] = {
    val parentTuple = new Apply2("", parent1, parent2, (t1: T1, t2: T2) => (t1, t2), collection)
    if (parentTuple.isCachable) new CachingChain(name, parentTuple, (p: (T1, T2)) => cpd(p._1, p._2), collection)
    else new NonCachingChain(name, parentTuple, (p: (T1, T2)) => cpd(p._1, p._2), collection)
  }

}

object CachingChain {

  /** Create a CachingChain of 1 argument. */
  def apply[T, U](parent: Element[T], cpd: T => Element[U])(implicit name: Name[U], collection: ElementCollection): CachingChain[T, U] =
    new CachingChain(name, parent, cpd, collection)

  /**
   * Create a CachingChain of 2 arguments. This is implemented as a chain on the single argument consisting of the pair of the two arguments.
   */
  def apply[T1, T2, U](parent1: Element[T1], parent2: Element[T2], cpd: (T1, T2) => Element[U])(implicit name: Name[U], collection: ElementCollection): CachingChain[(T1, T2), U] = {
    new CachingChain(
      name,
      new Apply2("", parent1, parent2, (t1: T1, t2: T2) => (t1, t2), collection),
      (pair: (T1, T2)) => cpd(pair._1, pair._2),
      collection)
  }
}

