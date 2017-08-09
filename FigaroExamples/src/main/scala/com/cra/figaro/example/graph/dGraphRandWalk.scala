/*
 * dGraphRandWalk.scala
 * Models the generative process of a random walk with restart.
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
import com.cra.figaro.example.graph._

/** Models the generative process of a random walk with restart. */
object dGraphRandWalk {

  def step(curr: Element[List[Int]], G: dGraph, init: Int, restart: Double): Element[Int] = {
    def fcn(t: List[Int], i: Int) = {
      val prob = G.Nodes(t.head).Edges.size
      val e = List.tabulate(prob)(j => (1.0 - restart) * (1.0 / prob.toDouble)).zip(G.Nodes(t.head).Edges.map(_.to).toList)
      Select((e ::: List((restart, i))): _*)
    }
    NonCachingChain(curr, (t: List[Int]) => fcn(t, init))
  }

  def RandWalk(Steps: Int, G: dGraph, init: Int, restart: Double): Element[List[Int]] = {
    if (Steps == 1) {
      val S = step(Constant(List(init)), G, init, restart)
      Apply(S, (t: Int) => List(t))
    } else {
      val prev = RandWalk(Steps - 1, G, init, restart)
      val curr = step(prev, G, init, restart)
      Apply(curr, prev, (t1: Int, t2: List[Int]) => List(t1) ::: t2)
    }
  }

  def apply(ParentSteps: Element[Int], RestartProb: Element[Double], ParentG: Element[dGraph])(implicit name: Name[List[Int]], collection: ElementCollection): NonCachingChain[Int, List[Int]] = {
    new NonCachingChain(
      name,
      ParentSteps,
      (t1: Int) => new NonCachingChain("", RestartProb,
        (t2: Double) => new NonCachingChain("", ParentG, (t3: dGraph) => RandWalk(t1, t3, t3.VoI.ID, t2), collection), collection),
      collection)
  }
}




