/*
 * MakeList.scala
 * An element representing making a list of a random number of random items.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 17, 2011
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.algorithm.ValuesMaker
import com.cra.figaro.algorithm.lazyfactored.{ ValueSet, LazyValues, Regular }
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.collection.mutable.Map
import com.cra.figaro.library.collection.VariableSizeArray
import com.cra.figaro.library.collection.FixedSizeArrayElement
import com.cra.figaro.library.collection.FixedSizeArray
import com.cra.figaro.library.collection.MakeArray

/**
 * An element representing making a list of a random number of random items.
 * The first argument is an element representing the number of items.
 * The second argument is an expression that generates an element representing an item.
 * MakeList is designed to store all the item elements and not change them as the number of elements changes.
 *
 * @param numItems The element representing the number of items in the list
 * @param itemMaker A function that creates an element representing a single item in the list
 * @deprecated("MakeList is deprecated. Please use the collections library for future support of MakeList capabilities", "3.2.1")
 */

@deprecated("MakeList is deprecated. Please use the collections library for future support of MakeList capabilities", "3.2.1")
class MakeList[T](name: Name[List[T]], vsa: FixedSizeArrayElement[T], collection: ElementCollection)
  extends Apply1[List[T], List[T]](name, vsa.foldLeft(List[T]())((c: List[T], n: T) => c :+ n), (l: List[T]) => l, collection) {
  
  val numItems = vsa.fsa.asInstanceOf[MakeArray[T]].numItems
  
  def items = vsa.fsa.value.generate(vsa.fsa.value.indices.toList).map(_._2).toStream
  
  def apply(i: Int) = i < vsa.fsa.value.size match {
    case true => vsa.fsa.value(i)
    case _ => throw new IllegalArgumentException("Invalid indices to MakeList")
  }

}

object MakeList {
  /**
   * Create a MakeList element using numItems to determine the number of items
   * and itemMaker to create each item in the list.
   */
  @deprecated("MakeList is deprecated. Please use the collections library for future support of MakeList capabilities", "3.2.1")
  def apply[T](numItems: Element[Int], itemMaker: () => Element[T])(implicit name: Name[List[T]], collection: ElementCollection) = {
    val vsa = VariableSizeArray(numItems, (i: Int) => itemMaker())("", collection)   
    new MakeList(name, vsa, collection)
  }
  
}


