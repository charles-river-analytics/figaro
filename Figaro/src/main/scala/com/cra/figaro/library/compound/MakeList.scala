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
import com.cra.figaro.algorithm.lazyfactored.{ValueSet, LazyValues, Regular}
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.collection.mutable.Map

/**
 * An element representing making a list of a random number of random items.
 * The first argument is an element representing the number of items.
 * The second argument is an expression that generates an element representing an item.
 * MakeList is designed to store all the item elements and not change them as the number of elements changes.
 *
 * @param numItems The element representing the number of items in the list
 * @param itemMaker A function that creates an element representing a single item in the list
 */
class MakeList[T](
  name: Name[List[T]],
  val numItems: Element[Int],
  val itemMaker: () => Element[T],
  collection: ElementCollection)
  extends Deterministic[List[T]](name, collection)
  with ValuesMaker[List[T]] with IfArgsCacheable[List[T]] {

  /**
   * An infinite stream of items in the list. At any point in time, the value of this element
   * is the prefix of items specified by the value of numItems.
   */
  lazy val items = Stream.continually({
    val item = itemMaker()
    universe.registerUses(this, item)
    item
  })

  override def args = numItems :: (items take numItems.value).toList

  override def generateValue = (items take numItems.value map (_.value)).toList

  /**
   * Return the i-th item in the list. Throws IllegalArgumentException if i is greater
   * than the current value of numItems
   */
  def apply(i: Int) = i < numItems.value match {
    case true => items(i)
    case _ => throw new IllegalArgumentException("Invalid indices to MakeList")
  }

  private def values = LazyValues(universe)

  /* We need to make sure that values are computed on the embedded Injects. Therefore, we create them in makeValues, store them, and use them in makeFactors.
   */
  val embeddedInject: Map[Int, Element[List[T]]] = Map()

  def makeValues(depth: Int): ValueSet[List[T]] = {
    // This code is subtle.
    // If we used itemMaker here, it would create bugs, as the items that appeared in the values would be different from the ones actually used by the Makelist.
    // On the other hand, if we used Values(items(0)) as a template for the values of all items, it would create other bugs, as different indices would have the same values,
    // even when the values include "new C", so they should all be different.
    // Therefore, we use Values()(items(i)) to get the possible value for each item in the stream.
    def possibleItemLists(length: Int): ValueSet[List[T]] = {
      val inject = Inject(items.take(length):_*)
      embeddedInject += length -> inject
     values(inject, depth - 1)
    }
    val possibleLengthValues = values(numItems, depth - 1)
    val possibleLengths = possibleLengthValues.regularValues
    val itemListsForLengths = possibleLengths.map(possibleItemLists(_))
    val resultValues =
      for {
        itemLists <- itemListsForLengths
        list <- itemLists.regularValues
      } yield list
    val incomplete = itemListsForLengths.exists(_.hasStar) || possibleLengthValues.hasStar
    if (incomplete) ValueSet.withStar(resultValues); else ValueSet.withoutStar(resultValues)
  }

//  def makeFactors: List[Factor[Double]] = {
//    val parentVar = Variable(numItems)
//    // We need to create factors for the items and the lists themselves, which are encapsulated in this MakeList
//    val regularParents = parentVar.range.filter(_.isRegular).map(_.value)
//    val maxItem = regularParents.reduce(_ max _)
//    val itemFactors = List.tabulate(maxItem)((i: Int) => Factory.make(items(i)))
//    val indexedResultElemsAndFactors =
//      for { i <- regularParents } yield {
//        val elem = embeddedInject(i)
//        val factors = Factory.make(elem)
//        (Regular(i), elem, factors)
//      }
//    val conditionalFactors =
//      parentVar.range.zipWithIndex map (pair =>
//        Factory.makeConditionalSelector(this, parentVar, pair._2, Variable(indexedResultElemsAndFactors.find(_._1 == pair._1).get._2)))
//    conditionalFactors ::: itemFactors.flatten ::: indexedResultElemsAndFactors.flatMap(_._3)
//  }

  override def isCachable = {

    if (itemMaker().isCachable == false) {
      false
    }

    for (arg <- args) yield {
      if (arg.isCachable == false) {
        false
      }
    }

    true
  }

}

object MakeList {
  /**
   * Create a MakeList element using numItems to determine the number of items
   * and itemMaker to create each item in the list.
   */
  def apply[T](numItems: Element[Int], itemMaker: () => Element[T])(implicit name: Name[List[T]], collection: ElementCollection) = {
    new MakeList(name, numItems, itemMaker, collection)
  }
}
