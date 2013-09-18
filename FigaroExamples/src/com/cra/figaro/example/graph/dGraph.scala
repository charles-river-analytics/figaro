/*
 * dGraph.scala
 * Contains class definitions for nodes, edges and graphs.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.example.graph

import com.cra.figaro.algorithm.decision.index._
import scala.collection.mutable.Set

/**
 * Contains class definitions for nodes, edges and graphs.
 */
class Node(val ID: Int) {
  var Edges = Set[Edge]()
  def degree = Edges.size
  def insertEdge(E: Edge) = Edges += E

  def copy() = {
    val N = new Node(ID)
    Edges.foreach(s => N.insertEdge(s.copy()))
    N
  }

  override def toString() = {
    ID + ": " + ("" /: Edges.map(_.toString()))((c: String, n: String) => c + n + ",")
  }
}
object Node {}

class Edge(val from: Int, val to: Int) {
  def copy() = new Edge(from, to)
  override def toString() = "(" + from + ", " + to + ")"
}
object Edge {}

class dGraph() extends Distance[dGraph] {

  def NodeCount = Nodes.size;
  def EdgeCount = Edges.size;

  // The vertex of interest (who we are giving a free product to)
  var VoI: Node = null

  var Nodes = Map[Int, Node]()
  var Edges = Set[Edge]()

  def copy(): dGraph = {
    val G = new dGraph()
    G.Nodes = Nodes.map(s => s._1 -> s._2.copy())
    G.Nodes.foreach(s => G.insertEdges(s._2))
    G.VoI = if (VoI != null) G.Nodes(VoI.ID) else null
    G
  }

  def insertNode(N: Node) = {
    Nodes += N.ID -> N
    insertEdges(N)
    val r = reverse(N.Edges)
    r.foreach { s =>
      insertEdge(s)
      Nodes(s.from).insertEdge(s)
    }
  }

  def insertEdge(E: Edge) = Edges += E
  def insertEdges(N: Node) = N.Edges.foreach(insertEdge(_))

  def reverse(E: Set[Edge]) = E.map(s => new Edge(s.to, s.from))

  def ClusterCoeff(N: Node) = {
    val L = N.Edges.map(_.to)
    if (L.size == 1) {
      0.0
    } else {
      val K = L.map(s => Nodes(s).Edges).flatten
      val T = (0 /: K)((c: Int, n: Edge) => {
        if (L.contains(n.to) && L.contains(n.from)) c + 1 else c
      })
      val c = T.toDouble / (L.size * (L.size - 1)).toDouble
      c
    }
  }

  /* The distance between graphs is the L2 norm of their cluster coefficient of the 
   * vertex of interest and the normalized number of edges at the vertex of interest
   */
  def distance(that: dGraph): Double = {
    val cc = math.pow(ClusterCoeff(VoI) - that.ClusterCoeff(that.VoI), 2.0)
    val v = math.pow((VoI.Edges.size - that.VoI.Edges.size) / math.max(NodeCount, that.NodeCount), 2.0)
    math.sqrt(cc + v)
  }

  override def toString(): String = {
    ("" /: Nodes.map(_._2.toString()))((c, n) => c + n + "\r\n")
  }

}

object dGraph {
  def apply() = new dGraph()
}

