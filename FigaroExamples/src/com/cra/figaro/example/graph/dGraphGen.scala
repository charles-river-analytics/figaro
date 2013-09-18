/*
 * dGraphGen.scala
 * Creates an element that represents the generative model of a graph.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.example.graph

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import scala.collection.mutable.Set

/**
 * Creates an element that represents the generative model of a graph.
 */

object dGraphGen {

  // Probability of edge attachment is a mix of preferential attachment and uniform attachment
  def probAttach(G: dGraph): List[(Double, Node)] = {
    val nNodes = G.NodeCount
    val nEdges = G.EdgeCount
    val prefattach = G.Nodes.map(s => (((s._2.degree + 1).toDouble / (nNodes + nEdges).toDouble), s._2)).toList
    val uniformattach = List.tabulate(nNodes)(i => 1.0 / nNodes.toDouble)
    // Mix 75% preferential attachment with 25% uniform attachment
    prefattach.zip(uniformattach).map(s => (0.75 * s._1._1 + 0.25 * s._2, s._1._2))
  }

  def addGraph(G: dGraph, NewEdges: List[Node], ID: Int): dGraph = {
    val N = new Node(ID)
    N.Edges = Set(NewEdges.distinct.map(s => new Edge(N.ID, s.ID)).toSeq: _*)
    val newG = G.copy()
    newG.insertNode(N)
    newG
  }

  // Generate a new graph from an old graph by adding a new node
  def genStep(G: Element[dGraph], NumNodes: Int, EdgeProb: Double): Element[dGraph] = {
    if (NumNodes == 0) {
      G
    } else {
      // Create an element for the number of edges to add from the new node
      val NumEdges = Geometric(EdgeProb)
      // Create the probability of attachment
      val Probabilities = Apply(G, probAttach)
      // Create an element to model the new edges
      val newEdges = dGraphEdgeGen(NumEdges, Probabilities)
      // Create a new graph by applying the addGraph function
      val newGraph = Apply(G, newEdges, (t1: dGraph, t2: List[Node]) => addGraph(t1, t2, NumNodes - 1))
      // Recursively add a new node
      genStep(newGraph, NumNodes - 1, EdgeProb)
    }
  }

  def genGraph(NumNodes: Int, Prob: Double) = {
    val initG = dGraph()
    initG.insertNode(new Node(NumNodes))
    val newG = genStep(Apply(Constant(0), (s: Int) => initG), NumNodes, Prob)
    newG
  }

  def apply(NumNodes: Element[Int], EdgeProb: Element[Double])(implicit name: Name[dGraph], collection: ElementCollection) = {
    new NonCachingChain(
      name,
      NumNodes,
      (t1: Int) => new NonCachingChain("", EdgeProb, (t2: Double) => genGraph(t1, t2), collection),
      collection)
  }

}


