/*
 * Node.scala  
 * Class definitions for nodes in a factor graph
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Oct 25, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.factors.graph

import com.cra.figaro.library.factors.{Factor, Variable}

/**
 * Trait for Nodes used in a Factor Graph
 */
trait Node {
  var parents: List[Node] = List[Node]()
  var children: List[Node] = List[Node]()
  
  def neighbors = parents ::: children
  
  def addParent(node: Node) = {
    parents :+= node
    parents
  }
  
  def removeParent(node: Node) = {
    parents = parents.filterNot(_ == node)
  }
  
  def addChild(node: Node) = {
    children :+= node
    children
  }
  
  def removeChild(node: Node) = {
    children = children.filterNot(_ == node)
  }
}

/**
 * Class for FactorNodes in a FactorGraph
 */
final case class FactorNode(val factor: Factor[_]) extends Node {
  override def toString() = "F(" + factor.variables.map(_.id).mkString(",") + ")"
}

/**
 * Class for VariableNodes in a FactorGraph
 */
case class VariableNode(val variable: Variable[_]) extends Node {
  override def toString() = "V(" + variable.id + ")"
}