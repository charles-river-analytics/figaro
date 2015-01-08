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
 * Chain is the common base class for caching and non-caching chains.
 * All chains have a cache, whose size is specified by the cacheSize argument.
 * When a parent value is encountered, first the cache is checked to see if the result element is known.
 * If it is not, the resulting element is generated from scratch by calling fcn.
 *
 * @param parent The parent element of the chain
 */

class Chain[T, U](name: Name[U], val parent: Element[T], fcn: T => Element[U], cacheSize: Int, collection: ElementCollection)
  extends Deterministic[U](name, collection) {
  def args: List[Element[_]] = if (active && resultElement != null) List(parent) ::: List(resultElement) else List(parent)

  protected def cpd = fcn

  /**
   * The type of the parent argument.
   */
  type ParentType = T

  /**
   * The current result element that arises from the current value of the parent.
   */
  var resultElement: Element[U] = _

  /* Data structures for the Chain. The cache stores previously generated result elements. The Context data
   * structures store the elements that were created in this context. We also stored newly created elements
   * in a subContext, which is based on the value of the parent.
   * Because Elements might be stored in sets or maps by algorithms, we need a way to allow the elements to be removed
   * from the set or map so they can be garbage collected. Universe provides a way to achieve this through the use of
   * contexts. When Chain gets the distribution over the child, it first pushes the context, and pops the context afterward, to mark
   * any generated elements as being generated in the context of this Chain.
   */
  lazy private[figaro] val cache: Map[T, Element[U]] = Map()
  lazy private[figaro] val myMappedContextContents: Map[T, Set[Element[_]]] = Map()
  lazy private[figaro] val elemInContext: Map[Element[_], T] = Map()

  private var lastParentValue: T = _

  /* Must override clear temporary for Chains. We can never leave the chain in an uninitialized state. That is,
   * the chain MUST ALWAYS have a valid element to return. So when clearing temporaries we clear everything
   * except the current context.
   */
  override def clearContext() = {
    myMappedContextContents.keys.foreach(c => if (c != lastParentValue) resizeCache(c))
  }

  /* Override context control for chain data structures */
  override def directContextContents: Set[Element[_]] = if (!active) {
    throw new NoSuchElementException
  } else Set(myMappedContextContents.values.flatten.toList: _*)

  override private[figaro] def addContextContents(e: Element[_]) = {
    myMappedContextContents(lastParentValue) += e
    elemInContext += (e -> lastParentValue)
  }

  override private[figaro] def removeContextContents(e: Element[_]) = {
    myMappedContextContents(elemInContext(e)) -= e
    elemInContext -= e
  }

  def generateValue() = {
    if (parent.value == null) parent.generate()
    val resultElem = get(parent.value)
    if (resultElem.value == null) resultElem.generate()
    resultElem.value
  }

  /* Computes the new result. If the cache contains a VALID element for this parent value, then return the
     * the element. Otherwise, we need to create one. First, we create an entry in the ContextContents since
     * any elements created in this context will be stored in the subContext of parentValue. Then, if the cache
     * is full, we resize the cache to make room for a new result element. Apply the function of the chain, and
     * stored the new result in the cache. If the element created is a new element, then registers it uses
     * in the Universe.
     */

  /**
   * Get the distribution over the result corresponding to the given parent value. Takes care of all bookkeeping including caching.
   */
  def get(parentValue: T): Element[U] = {
    val lruParent = lastParentValue
    lastParentValue = parentValue
    val cacheValue = cache.get(parentValue)

    val newResult =
      if (!cacheValue.isEmpty && cacheValue.get.active) cacheValue.get
      else {
        myMappedContextContents += (parentValue -> Set())
        if (cache.size >= cacheSize && cacheValue.isEmpty) {
          val dropValue = if (cache.last._1 != lruParent) cache.last._1 else cache.head._1
          resizeCache(dropValue)
        }
        val result = getResult(parentValue)
        cache += (parentValue -> result)
        universe.registerUses(this, result)
        result
      }

    resultElement = newResult
    newResult
  }

  /**
   * Get the distribution over the result corresponding to the given parent value. This call is UNCACHED,
   * meaning it will not be stored in the Chain's cache, and subsequent calls using the same parentValue
   * could return different elements.
   */
  def getUncached(parentValue: T): Element[U] = {
    if (lastParentValue == null || lastParentValue != parentValue) {
      myMappedContextContents.getOrElseUpdate(parentValue, Set())
      lastParentValue = parentValue
      resultElement = getResult(parentValue)
      universe.registerUses(this, resultElement)
    }
    resultElement
  }

  // All elements created in cpd will be created in this Chain's context with a subContext of parentValue
  private def getResult(parentValue: T): Element[U] = {
    universe.pushContext(this)
    val result = cpd(parentValue)
    universe.popContext(this)
    result
  }

  /* Current replacement scheme just drops the last element in the cache. The dropped element must be deactivated,
   * and removed from the context data structures.
   */
  protected def resizeCache(dropValue: T) = {
    cache -= dropValue
    if (myMappedContextContents.contains(dropValue)) {
      universe.deactivate(myMappedContextContents(dropValue))
      elemInContext --= myMappedContextContents(dropValue)
      myMappedContextContents -= dropValue
    }
  }

  override def toString = "Chain(" + parent + ", " + cpd + ")"
}

/**
 * A NonCachingChain is an implementation of Chain with a single element cache.
 */
class NonCachingChain[T, U](name: Name[U], parent: Element[T], cpd: T => Element[U], collection: ElementCollection)
  extends Chain(name, parent, cpd, 2, collection)

/**
 * A CachingChain is an implementation of Chain with a 1000 element cache.
 */
class CachingChain[T, U](name: Name[U], parent: Element[T], cpd: T => Element[U], collection: ElementCollection)
  extends Chain(name, parent, cpd, Int.MaxValue, collection)

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

