/*
 * BasicFactorGraph.scala  
 * A basic factor graph for double factors
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 15, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.beliefpropagation

import scala.collection.Iterable
import scala.collection.immutable.List
import com.cra.figaro.algorithm.factored.factors._

/**
 * The basic implementation of FactorGraph for Probabilistic factors in BP.
 */
class BasicFactorGraph(factors: List[Factor[Double]], semiring: Semiring[Double])
  extends FactorGraph[Double] {

  def uniformFactor(v: List[Variable[_]]): Factor[Double] = {
    val f = new BasicFactor[Double](List(), v)
    f.fillByRule((l: List[Any]) => semiring.one)
    f
  }

  // Combine all factors of the same variables into a single factor
  private def combineFactors() = factors.groupBy(_.variables.map(_.id)).values.map(_.reduceLeft(_.product(_, semiring))).toList

  /*
   * Create Nodes for factors
   */
  private def adjacencyListFactors(): Map[Node, Map[Node, Factor[Double]]] = {
    factorsByNode.map { factors =>
      (factors._1 -> {
        Map[Node, Factor[Double]]() ++ factors._2.variables.map(v => VariableNode(v) -> uniformFactor(List(v)))
      })
    }
  }

  /*
   * Create Nodes for Variables
   */
  private def adjacencyListVariables(): Map[Node, Map[Node, Factor[Double]]] = {
    val adjacencyListVariables = factorsByNode.map(f => f._2.variables.map(v => VariableNode(v) -> f._1)).flatten

    // Group them by common variables
    val adjacencyListGrouped = adjacencyListVariables.groupBy(_._1)

    Map[Node, Map[Node, Factor[Double]]]() ++ adjacencyListGrouped.map(e => {
      e._1 -> (Map[Node, Factor[Double]]() ++ e._2.map(f => f._2 -> uniformFactor(List(f._1.variable))))
    })
  }

  def toMutableMap(m: Map[Node, Factor[Double]]): scala.collection.mutable.Map[Node, Factor[Double]] =
    scala.collection.mutable.Map[Node, Factor[Double]]() ++ m

  private[figaro] val factorsByNode = combineFactors.map(factor => (new FactorNode(factor.variables.toSet) -> (factor))).toMap

  private[figaro] val adjacencyList = (adjacencyListFactors() ++ adjacencyListVariables()).map(m => m._1 -> toMutableMap(m._2))

  /**
   * Returns all nodes in the factor graph.
   */
  def getNodes(): Iterable[Node] = adjacencyList.keys

  /**
   * Returns all neighbors of a given node.
   */
  def getNeighbors(source: Node): Iterable[Node] = adjacencyList(source).keys

  /**
   * Returns all neighbors of a given node excluding the node of the second argument.
   */
  def getNeighbors(source: Node, excluding: Node): Iterable[Node] = getNeighbors(source).filterNot(_ == excluding)

  /**
   * Gets the factor for a particular factor node.
   */
  def getFactorForNode(fn: FactorNode): Factor[Double] = factorsByNode(fn)

  /**
   * Get a list of messages to the node.
   */
  def getMessagesForNode(node: Node): Iterable[(Node, Factor[Double])] = adjacencyList(node)

  /**
   * Gets the last message to a node from another.
   */
  def getLastMessage(from: Node, to: Node): Factor[Double] = adjacencyList(from)(to)

  /**
   * Updates the factor graph with a message from a node to another.
   * Returns a new factor graph, which can be the same as this one.
   */
  def update(from: Node, to: Node, f: Factor[Double]): FactorGraph[Double] = {
    adjacencyList(from) += (to -> f)
    this
  }

  def contains(v: Node): Boolean = adjacencyList.contains(v)
}