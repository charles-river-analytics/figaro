/*
 * VPIndex.scala
 * A VP Index for decisions
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.algorithm.decision.index

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import scala.collection.mutable.{ HashMap, MultiMap, Set, PriorityQueue }
import scala.collection.immutable.Map
import scala.collection.immutable.{ List, SortedMap }
import math.{ min, max }

/**
 * VP index leaf node class
 */
private[index] class VPlnode[T, U](parent: Node[_, _]) extends Node[T, U](parent, true) with LNode[T, U]

/**
 * VP index internal node class
 */
private[index] class VPinode[T, U](parent: Node[_, _], val pivot: Distance[T], val radius: Double) extends Node[T, U](parent, false) with INode[T, U] {

  def lchild = children.head
  def rchild = children.last

  /*
   * Internal distance in a VP-index is a lower bound between the distance between the query
   * and the pivot point and the radius of the node
   */
  override def iDist(v: T): Iterable[(Double, Node[T, U])] = {
    var m = List[(Double, Node[T, U])]()
    val d = pivot.distance(v)
    m = m :+ (max(d - radius, 0.0), lchild)
    m = m :+ (max(radius - d, 0.0), rchild)
    m
  }
}

/**
 * A VP index to compute nearest neighbor queries. This is the metric space equivalent of a k-d tree. Given a query value
 * the index performs a best first search through the index until the guaranteed k nearest neighbors are found.
 * See Foundations of Multidimensional and Metric Data Structures by Hanan Samet for more details
 * 
 *  @tparam T The type of the parent
 *  @tparam U The type of the decision 
 *  @param stratMap A mapping from parent/decision values to observed utilities. This is output
 *  by a DecisionAlgorithm.
 * 
 */
class VPIndex[T <% Distance[T], U](stratMap: Map[(T, U), DecisionSample], capacity: Int) extends Index[T, U](stratMap) {

  private def pivotSamples = 0.1

  private var tree = split(null, remap)

  /**
   * Return the k nearest neighbor samples from the parent value
   */
  def getNN(parent: T, num: Int): List[(Double, U, DecisionSample)] = {
    val NNs = getNNDebug(parent, num).toList
    val temp = NNs.flatMap { s => List.tabulate(s._2.size)(i => s._1).zip(s._2.toList) }
    temp.map { s => (s._1, s._2._1, s._2._2) }
  }

  /**
   * The debug version of the getNN returns the raw data from the index node.
   */
  override def getNNDebug(parent: T, num: Int): Iterable[(Double, Set[(U, DecisionSample)])] = {

    val NodeQueue = PriorityQueue[(Double, Node[T, (U, DecisionSample)])]()(Ordering.by(-1.0 * _._1))
    val NNQueue = PriorityQueue[(Double, Set[(U, DecisionSample)])]()(Ordering.by(_._1))

    if (tree.leaf) {
      tree.oDist(parent).toList.sortBy(_._1).take(num)
    } else {
      NodeQueue.enqueue((0, tree))

      // Loop while the queue of tree nodes is not empty, or the lower bound in the tree is still
      // less than the k-th nearest neighbor
      while (NodeQueue.nonEmpty && (NNQueue.size < num || NodeQueue.head._1 < NNQueue.head._1)) {
        val first = NodeQueue.dequeue()
        // if the head of the queue is a leaf node, compute the distance from the query to all the object in the node
        if (first._2.leaf) {
          val a = first._2.oDist(parent)
          a.foreach { s =>
            // if the object distance is less than the k-th NN, add it to the NN queue and pop the highest value
            if (NNQueue.size < num || s._1 < NNQueue.head._1) {
              NNQueue += s // 
              if (NNQueue.size > num) NNQueue.dequeue()
            }
          }
          // if the head is an internal node, compute the lower bound distance to the children of the node
        } else {
          first._2.iDist(parent).foreach(s => if (NNQueue.size < num || s._1 < NNQueue.head._1) NodeQueue += s)
        }
      }
      NNQueue
    }
  }

  private def remap = stratMap.toList.map(s => (s._1._1, (s._1._2, s._2)))

  // Compute the mean distance of a set of points
  private def mean(c: Distance[T], pts: List[T]) = (pts.map { c.distance(_) }.sum) / pts.length.toDouble

  // Compute the variance distance of a set of points
  private def variance(c: Distance[T], pts: List[T]) = {
    val m = mean(c, pts)
    (pts.map { s => math.pow(m - c.distance(s), 2.0) }.sum) / pts.length.toDouble
  }

  // compute a new pivot point from a set of samples. 
  private def findPivot(samples: List[(T, (U, DecisionSample))]) = {
    val numrand = max(1, pivotSamples * samples.length.toDouble).toInt
    val rpivot = random.shuffle(samples).take(numrand)
    val rsamples = random.shuffle(samples).take(numrand).unzip._1
    val maxvarindex = (rpivot.map(s => variance(s._1, rsamples))).zipWithIndex.max._2
    rpivot(maxvarindex)
  }

  // Split a node with too many children
  private def split(parent: Node[_, _], samples: List[(T, (U, DecisionSample))]): Node[T, (U, DecisionSample)] = {
    if (samples.length < capacity) {
      val n = new VPlnode[T, (U, DecisionSample)](parent)
      samples.foreach { s => n.addObject(s._1, s._2) }
      n
    } else {
      val pivot = findPivot(samples)
      val pivotdistance = samples.map(s => pivot._1.distance(s._1)).zip(samples)
      val sortedDs = pivotdistance.sortBy(_._1)
      val medianindex = sortedDs.length / 2
      val (left, right) = sortedDs.unzip._2.splitAt(medianindex)

      val newnode = new VPinode[T, (U, DecisionSample)](parent, pivot._1, sortedDs(medianindex)._1)
      newnode.addChild(split(newnode, left))
      newnode.addChild(split(newnode, right))
      newnode
    }
  }
}


object VPIndex {
  /**
   * Create a VP index from a map of (parent, decision) -> DecisionSamples with the given leaf node capacity
   */
  def apply[T <% Distance[T], U](strat: Map[(T, U), DecisionSample], capacity: Int) = {
    new VPIndex(strat, capacity)
  }
}














