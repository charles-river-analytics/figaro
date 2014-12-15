/*
 * Fold.scala
 * Class for TBD
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Nov 27, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._
import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.{LazyValues, ValueSet}

/**
 * Doc needed
 */
class FoldLeft[T,U](name: Name[U], val start: U, val function: (U, T) => U, val elements: Seq[Element[T]], collection: ElementCollection)
extends Deterministic[U](name, collection) with ValuesMaker[U] {
  def args = elements.toList
  override def generateValue = elements.map(_.value).foldLeft(start)(function)

  def makeValues(depth: Int): ValueSet[U] = {
    val values = LazyValues(universe)

    def helper(currentAccum: ValueSet[U], remainingElements: Seq[Element[T]]): ValueSet[U] = {
      if (remainingElements.isEmpty) currentAccum
      else {
        val firstVS = values(remainingElements.head, depth - 1)
        val nextRegular =
          for {
            currentAccumVal <- currentAccum.regularValues
            firstVal <- firstVS.regularValues
          } yield function(currentAccumVal, firstVal)
        val nextHasStar = currentAccum.hasStar || firstVS.hasStar
        val nextAccum = if (nextHasStar) ValueSet.withStar(nextRegular) else ValueSet.withoutStar(nextRegular)
        helper(nextAccum, remainingElements.tail)
      }
    }

    helper(ValueSet.withoutStar(Set(start)), elements)
  }
}

object FoldLeft  {
  /*
   * FoldLeft has two alternative implementations.
   * For factored algorithms, we use a decomposition into a chain series.
   * For other algorithms, we use a simple generateValue.
   */
  def apply[T,U](start: U, function: (U, T) => U)(elements: Element[T]*)(implicit name: Name[U], collection: ElementCollection): Element[U] = {
    if (elements.isEmpty) Constant(start)
    else {
      val elem = elements.head
      if (!elem.active) elem.activate
      new FoldLeft(name, start, function, elements, collection)
    }
  }
}

object FoldRight  {
  def apply[T,U](start: U, function: (T, U) => U)(elements: Element[T]*)(implicit name: Name[U], collection: ElementCollection): Element[U] = {
    FoldLeft(start, (u: U, t: T) => function(t, u))(elements.reverse:_*)(name, collection)
  }
}

object Reduce {
  def apply[T](function: (T, T) => T)(elements: Element[T]*)(implicit name: Name[T], collection: ElementCollection): Element[T] = {
    val elem = elements.head
    if (!elem.active) elem.activate
    Chain(elem, (t: T) => FoldLeft(t, function)(elements.tail:_*))(name, collection)
  }
}
