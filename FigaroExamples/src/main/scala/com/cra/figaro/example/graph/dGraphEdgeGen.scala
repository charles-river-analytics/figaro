/*
 * dGraphEdgeGen.scala
 * Edge generation model.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.graph

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.util._

/**
 * Edge generation model. To generate an edge, we create a chain from the number of edges and the
 * probability connecting to each node, and create an Inject node. Each edge is modeled as a distribution
 * over the nodes, and the inject converts the list of elements to an element of a list of edges
 */
object dGraphEdgeGen {

  def genEdge(NumEdges: Int, Probabilities: List[(Double, Node)]) = {
    val e = for { i <- 0 until NumEdges } yield Select(Probabilities: _*)
    Inject(e: _*)
  }

  def apply(NumEdges: Element[Int], Probabilities: Element[List[(Double, Node)]])(implicit name: Name[List[Node]], collection: ElementCollection): NonCachingChain[Int, List[Node]] = {
    new NonCachingChain(
      name,
      NumEdges,
      (t1: Int) => new NonCachingChain("", Probabilities, (t2: List[(Double, Node)]) => genEdge(t1, t2), collection),
      collection)
  }
}
