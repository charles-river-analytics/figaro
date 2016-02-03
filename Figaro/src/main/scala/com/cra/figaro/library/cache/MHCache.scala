/*
 * MHCache.scala
 * Implements caching for caching and non-caching chains, specifically designed for MH.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.library.cache

import com.cra.figaro.language._
import scala.collection.mutable.Map
import scala.collection.mutable.Set

/**
 * A class which implements caching for caching and non-caching chains, specifically designed for MH
 *
 * For caching chains, the result of the Chain's function is cached for each value of the parent element
 * that is queried. This cache is infinitely large.
 *
 * For non-caching chains, we only "cache" two resulting elements of the chain. The cache is actually
 * a 2-element stack, where the top of the stack represents the most recent element for the chain, and the
 *  bottom of the stack represents the last element (and parent value) used. This is primarily to benefit
 *  MH; if a proposal is rejected, we want to switch a chain back to where it was without much overhead.
 *
 */
class MHCache(universe: Universe) extends Cache(universe) {

  /* Caching chain cache that maps from an element to a map of parent values and resulting elements */
  private[figaro] val ccCache: Map[Element[_], Map[Any, Element[_]]] = Map()

  /* The inverted cache. This maps from result elements back to the chain that uses them. This is needed
   * to properly clean up deactivated elements 
   */
  private[figaro] val ccInvertedCache: Map[Element[_], Map[Element[_], Any]] = Map()

  /*
   * The non-caching chain "cache". This is a map from elements to a list of:
   * (parent value, result element, Set of elements created in the context of the parent value)
   * The Set is needed since once a parent value falls off the stack, we have to clear all the elements
   * created in the context of that parent value or else we will have memory leaks
   */
  private[figaro] val nccCache: Map[Element[_], List[(Any, Element[_], Set[Element[_]])]] = Map()

  /**
   * Retrieve any cached element generated from the current value of the supplied element. Returns None if
   * the element does not generate another element.
   *
   */
  def apply[T](element: Element[T]): Option[Element[T]] = {
    element match {
      case c: CachingChain[_, T] => {
        doCachingChain(c)
      }
      case c: NonCachingChain[_, T] => {
        doNonCachingChain(c)
      }
      case _ => None
    }
  }

  /*
   * Retrieves an element from the caching chain cache, or inserts a new one if none is found for
   * the value of this element
   */
  private def doCachingChain[U, T](c: CachingChain[U, T]): Option[Element[T]] = {

    val cachedElems = ccCache.getOrElseUpdate(c, Map())
    val cachedValue = cachedElems.get(c.parent.value)
    if (!cachedValue.isEmpty) cachedValue.asInstanceOf[Option[Element[T]]]
    else {
      // If the value of the element is not found in the cache, generate a new element by calling the chain,
      // add it to the cache and the inverted value, and return
      val result = c.get(c.parent.value)
      cachedElems += (c.parent.value -> result)
      val invertedElems = ccInvertedCache.getOrElseUpdate(result, Map())
      invertedElems += (c -> c.parent.value)
      Some(result)
    }
  }

  /*
   * Retrieves an element for a non-caching chain. This is not really a cache but rather,
   * for each element, a 2-deep stack is maintained that has the current result element of the chain
   * at the top of the stack, and the last result element at the bottom of the stack. This is for use in 
   * MH. When a proposal is made, the chain may change its value. In such a case, we don't want to lose
   * the current result element in case the proposal is rejected, so it is moved to the back of the stack.
   * If the proposal is reject, the chain is regenerated and the old element is restored to the top of the stack.
   * 
   */
  private def doNonCachingChain[U, T](c: NonCachingChain[U, T]): Option[Element[T]] = {
    val nccElems = nccCache.getOrElse(c, List())

    if (nccElems.isEmpty) {
      // If no element has been stored for this chain, generate a value and store it in the stack for this element
      val result = c.get(c.parent.value)
      nccCache += (c -> List((c.parent.value, result, Set())))
      Some(result)
    } else if (c.parent.value == nccElems.head._1) {
      // If the current value of the parent matches the value at the top of the stack, return the element at the top of the stack
      Some(nccElems.head._2.asInstanceOf[Element[T]])
    } else if (nccElems.size > 1 && c.parent.value == nccElems.last._1) {
      // If the current value matches the value at the bottom of the stack, then we need to do a swap; move the back to the front
      // and the front to the back
      
      // Store the elements in the context of the top of the stack. This is the current context of the chain minus the context
      // of the value at the back of the stack
      val oldContext = c.directContextContents.clone -- nccElems.last._3
      // swap the head and last positions
      val head = (nccElems.last._1, nccElems.last._2, Set[Element[_]]())
      val last = (nccElems.head._1, nccElems.head._2, oldContext)
      nccCache += (c -> List(head, last))
      Some(nccElems.last._2.asInstanceOf[Element[T]])
    } else {
      // Otherwise, we have a new parent value. In this case, we drop the bottom of the stack, deactivate the element
      // and the context of the value we are dropping.
      if (nccElems.size == 2) universe.deactivate(nccElems.last._3)
      val oldContext = c.directContextContents.clone
      val result = c.get(c.parent.value)
      val head = (c.parent.value, result, Set[Element[_]]())
      val last = (nccElems.head._1, nccElems.head._2, oldContext)
      nccCache += (c -> List(head, last))
      Some(result)
    }
  }

  /**
   * Removes an element from the cache. This is needed to properly clean up elements as they are deactivated.
   */
  def -=(element: Element[_]) = {
    ccCache -= element
    nccCache -= element
    val invertValue = ccInvertedCache.get(element)
    if (invertValue.nonEmpty) invertValue.get.foreach(e => if (ccCache.contains(e._1)) ccCache(e._1) -= e._2)
    ccInvertedCache -= element
    this
  }

  /**
   * Clears the cache of all stored elements.
   */
  def clear() = {
    ccCache.clear()
    ccInvertedCache.clear()
    nccCache.clear()
    universe.deregister(this)
  }
}