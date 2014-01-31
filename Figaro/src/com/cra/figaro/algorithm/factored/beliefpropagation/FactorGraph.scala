/*
 * FactorGraph.scala  
 * Factor Graph trait used for BP
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

import com.cra.figaro.algorithm.factored._

trait FactorGraph[T] {
 
  def uniformFactor(v: List[Variable[_]]): Factor[T]
  
  def getNodes(): Iterable[Node]
  
  def getNeighbors(source: Node): Iterable[Node]

  def getNeighbors(source: Node, excluding: Node): Iterable[Node]
    
  def getFactorForNode(fn: FactorNode): Factor[T]
  
  def getMessagesForNode(node: Node): Iterable[(Node, Factor[T])]
  
  def getLastMessage(from: Node, to: Node): Factor[T]
  
  def update(from: Node, to: Node, f: Factor[T]): FactorGraph[T]
  
}