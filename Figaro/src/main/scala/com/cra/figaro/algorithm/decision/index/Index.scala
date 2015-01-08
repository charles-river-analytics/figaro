/*
 * Index.scala
 * Base class for decision index
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

import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import scala.collection.immutable.{ SortedMap, Map }
import scala.collection.mutable.{ HashMap, MultiMap, Set, PriorityQueue }

/*
 * To create a new index class, it must inherit from the Index base class and implement getNN(parent value, num neighbors).
 * The parent type of the index is assumed to be the Distance[T] class, or a class in which an implicit conversion is
 * available for T->Distance[T]. The implementation of getNN is up to the user.
 *
 * Classes for nodes of an index tree are also provided for convenience.
 */
/**
 * Abstract base class for indices used to retrieve nearest neighbors in an approximate decision strategy.
 * 
 *  @tparam T The type of the parent
 *  @tparam U The type of the decision
 *  @param stratMap A mapping from parent/decision values to observed utilities. This is output
 *  by a DecisionAlgorithm.
 */
abstract class Index[T <% Distance[T], U](stratMap: Map[(T, U), DecisionSample]) {
  
  /* Implementors must define this class */
  /**
   * Get the specified number of nearest neighbors of the parent value. Returns a tuple list of
   * (distance to parent, decision value, weighted utility).
   */
  def getNN(parent: T, num: Int): List[(Double, U, DecisionSample)]
  
  /*
   * Debug version of getNN. Not defined by default, specific implementations can override
   */
  protected def getNNDebug(parent: T, num: Int): Iterable[(Double, Set[(U, DecisionSample)])] = null
  
  /**
   * Get the size of the index.
   */
  def size = stratMap.size
}

/**
 * Helper class to create indices. Defines a node in a tree, with a parent node and a boolean
 * indication if it is a leaf node.
 * 
 * @param leaf Indicates if this is a leaf node
 */
abstract class Node[T, U](parent: Node[_, _], val leaf: Boolean) {

  protected type ObjectMapType = HashMap[Distance[T], Set[U]] with MultiMap[Distance[T], U]

  /**
   *  Defines the distance between a query and the internal nodes of an index.
   */   
  def iDist(v: T): Iterable[(Double, Node[T, U])]

  /**
   *  Defines the distance between a query and the leaf nodes of an index.
   */ 
  def oDist(v: T): Iterable[(Double, Set[U])]

  /**
   * The children of a node.
   */
  var children: List[Node[T, U]] = List()
  
  /**
   * Adds a child node to this node.
   */
  def addChild(n: Node[T, U]) = children = children :+ n
}

/** Convenience trait for internal node of a tree. */
trait INode[T, U] extends Node[T, U] {
  def oDist(o: T) = Map[Double, Set[U]]()
}

/** Convenience trait for leaf node of a tree. */
trait LNode[T, U] extends Node[T, U] {
  val objects: ObjectMapType = new HashMap[Distance[T], Set[U]] with MultiMap[Distance[T], U]
  
  /**
   * Adds an object of Distance[T] to the node that contains a value v of type U.
   */
  def addObject(k: Distance[T], v: U) = objects.addBinding(k, v)

  /**
   * Compute the distance between each object stored in the leaf node
   * and a query value of type Distance[T]. Will return the distances in sorted order.
   */
  def oDist(o: T): Iterable[(Double, Set[U])] = {
    val m = PriorityQueue[(Double, Set[U])]()(Ordering.by(-1.0 * _._1))
    objects.foreach { s: (Distance[T], Set[U]) => m += ((s._1.distance(o), s._2)) }
    m
  }
  def iDist(o: T) = Map[Double, Node[T, U]]()
}













