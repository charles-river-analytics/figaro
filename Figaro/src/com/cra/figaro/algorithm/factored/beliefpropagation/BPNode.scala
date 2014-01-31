/*
 * BPNode.scala  
 * Class definitions for nodes in a factor graph
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

import com.cra.figaro.algorithm.factored.Variable

trait Node

final class FactorNode(val variables: List[Variable[_]]) extends Node {
  override def toString() = "F(" + variables.map(_.id).mkString(",") + ")"
}

case class VariableNode(val variable: Variable[_]) extends Node {
  override def toString() = "V(" + variable.id + ")"
}