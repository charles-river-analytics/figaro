package com.cra.figaro.util

import com.cra.figaro.language._
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.generic.Shrinkable

/** 
 * Abstract class to manage caching of element generation for a universe. This class can be used
 * by algorithms to manage caching of chains.
 */ 
abstract class Cache(universe: Universe) extends Shrinkable[Element[_]] {

  /**
   * Return the next element from the generative process defined by element. If no process
   * is found, return None
   */
  def apply[T](element: Element[T]): Option[Element[T]]  

  universe.register(this)

  /**
   * Clear any caching
   */
  def clear(): Unit
}

/** A Cache class which performs no caching */
class NoCache(universe: Universe) extends Cache(universe) {
  def apply[T](element: Element[T]): Option[Element[T]] = None
  def clear() = {}
  def -=(element: Element[_]) = this
}

/**
 * A class which implements caching for caching and non-caching chains.
 * 
 */
class ChainCache(universe: Universe) extends Cache(universe) {

  val ccCache: Map[Element[_], Map[Any, Element[_]]] = Map()
  val ccInvertedCache: Map[Element[_], Map[Element[_], Any]] = Map()

  val nccCache: Map[Element[_], List[(Any, Element[_], Set[Element[_]])]] = Map()

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

  def doCachingChain[U, T](c: CachingChain[U, T]): Option[Element[T]] = {
    val cachedElems = ccCache.getOrElseUpdate(c, Map())
    val cachedValue = cachedElems.get(c.parent.value)
    if (!cachedValue.isEmpty) cachedValue.asInstanceOf[Option[Element[T]]]
    else {
      val result = c.get(c.parent.value)
      cachedElems += (c.parent.value -> result)
      val invertedElems = ccInvertedCache.getOrElseUpdate(result, Map())
      invertedElems += (c -> c.parent.value)
      Some(result)
    }
  }

  def doNonCachingChain[U, T](c: NonCachingChain[U, T]): Option[Element[T]] = {
    val nccElems = nccCache.getOrElse(c, List())
    if (nccElems.isEmpty) {
      val result = c.get(c.parent.value)
      nccCache += (c -> List((c.parent.value, result, Set())))
      Some(result)
    } else if (c.parent.value == nccElems.head._1) Some(nccElems.head._2.asInstanceOf[Element[T]])
    else if (nccElems.size > 1 && c.parent.value == nccElems.last._1) {
      val oldContext = c.directContextContents.clone -- nccElems.last._3
      val head = (nccElems.last._1, nccElems.last._2, Set[Element[_]]())
      val last = (nccElems.head._1, nccElems.head._2, oldContext)
      //oldContext.foreach(c.removeContextContents(_))
      nccCache += (c -> List(head, last))
      Some(nccElems.last._2.asInstanceOf[Element[T]])
    } else {
      if (nccElems.size == 2) universe.deactivate(nccElems.last._3)
      val oldContext = c.directContextContents.clone
      //oldContext.foreach(c.removeContextContents(_))
      val result = c.get(c.parent.value)
      val head = (c.parent.value, result, Set[Element[_]]())
      val last = (nccElems.head._1, nccElems.head._2, oldContext)
      nccCache += (c -> List(head, last))
      Some(result)
    }
  }

  /*
  def set(element: Element[_], result: Element[_]): Unit = {
    element match {
      case c: CachingChain[_, _] => {
        ccCache.getOrElseUpdate(c, Map()) += (c.parent.value -> result)
      }
      case c: NonCachingChain[_, _] => {

      }
      case _ => ()
    }
  }
  * 
  */

  def -=(element: Element[_]) = {
    ccCache -= element
    nccCache -= element
    val invertValue = ccInvertedCache.get(element)
    if (invertValue.nonEmpty) invertValue.get.foreach(e => ccCache(e._1) -= e._2)
    ccInvertedCache -= element
    this
  }

  def clear() = {
    ccCache.clear()
    ccInvertedCache.clear()
    nccCache.clear()
    universe.deregister(this)
  }
}

