/*
 * FlatIndex.scala
 * A Flat index for decisions
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.decision.index

import com.cra.figaro.algorithm._
import com.cra.figaro.library.decision._
import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.language._
import scala.collection.immutable.Map
import scala.collection.mutable.{ HashMap, MultiMap, Set }

/**
 * Node class used in the flat index.
 */
private[index] class FlatNode[T, U] extends Node[T, U](null, true) with LNode[T, U]

/**
 * FlatIndex class implements a nearest neighbor index that simply computes the distance
 * between the query and all the stored samples, and returns the k nearest values using a simple sort.
 * 
 *  @tparam T The type of the parent
 *  @tparam U The type of the decision 
 *  @param stratMap A mapping from parent/decision values to observed utilities. This is output
 *  by a DecisionAlgorithm.
 */
class FlatIndex[T <% Distance[T], U](stratMap: Map[(T, U), DecisionSample]) extends Index[T, U](stratMap) {

  private val db = toFlatNode(stratMap)

  def getNN(parent: T, num: Int): List[(Double, U, DecisionSample)] = {
    val NNs = db.oDist(parent).toList.sortBy(_._1).take(num)
    val temp = NNs.flatMap { s => List.tabulate(s._2.size)(i => s._1).zip(s._2.toList) }
    temp.map { s => (s._1, s._2._1, s._2._2) }
  }

  /*
   * Will return a non-flattened version of the nearest neighbor results
   */
  override protected def getNNDebug(parent: T, num: Int): Iterable[(Double, Set[(U, DecisionSample)])] = {
    val NNs = db.oDist(parent).toList.sortBy(_._1).take(num)
    NNs
  }

  private def toFlatNode(strat: Map[(T, U), DecisionSample]): FlatNode[T, (U, DecisionSample)] = {
    val db = new FlatNode[T, (U, DecisionSample)]
    strat.foreach(k => db.addObject(k._1._1, (k._1._2, k._2)))
    db
  }

}


object FlatIndex {
  /**
 * Create a flat index from a map of (parent, decision) -> Decision values.
 */
  def apply[T <% Distance[T], U](strat: Map[(T, U), DecisionSample]) = {
    new FlatIndex(strat)
  }
}












