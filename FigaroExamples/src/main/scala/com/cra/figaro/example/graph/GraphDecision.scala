/*
 * GraphDecision.scala
 * An example that uses a graph as the parent of a decision.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example.graph

import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.Predicate
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.decision._
import com.cra.figaro.example.graph._

/**
 * An example that uses a graph as the parent of a decision. This scenario is meant to simulate
 * a decision to give a free product to a person in a social network, where the value of the free
 * product is the number of unique people in the user's network that interact with the product
 * (modeled as a random walk)
 *
 * The network generation model is a mix of preferential attachment and uniform random attachment.
 *
 * Note this can take a long time to run (15+ minutes)
 */

object GraphDecision {

  /* Number of nodes in the graph */
  val NumNodes = Constant(10)

  /* Number of edges modeled as a geometric distribution, set the probability of failure */
  val Prob = Constant(.75)

  /* An Element[dGraph] that generates a graph using node and edge generation parameters */
  val GraphGenerator = dGraphGen(NumNodes, Prob)

  /* An Element[dGraph], that from an input graph, select a node uniformly at random to be the vertex of interest
   * (ie, the person the product is given to) */
  val VoISelect = Chain(GraphGenerator, (G: dGraph) => Uniform((0 until G.NodeCount): _*))
  def voifcn(v: Int, g: dGraph) = {
    val G = g.copy()
    G.VoI = G.Nodes(v)
    G
  }
  val GraphVoI = Apply(VoISelect, GraphGenerator, voifcn)

  /* Number of steps in the random walk, set at 80% of the number of nodes in the graph */
  val NumSteps = Apply(NumNodes, (i: Int) => (0.8 * i).toInt)

  /* Restart probability of random walk */
  val RestartProb = Constant(0.33)

  /* An Element[List[Int]], which the steps in a random  walk
   * over the input graph, with the specified number of steps, restart probability
   * and starting node */
  val GraphRandWalk = dGraphRandWalk(NumSteps, RestartProb, GraphVoI)

  /* An Element[Int], which is the distinct number of nodes in the random walk */
  val RWalkFcn = Apply(GraphRandWalk, (t: List[Int]) => t.distinct.length)

  /* A boolean decision Element[dGraph] that decides whether to give a product to a person only on the
   * observation of their social network.
   * 
   * We will use the default strategy for a non-caching decision, which is an approximate strategy.
   * The distance between two graphs is the L2 distance between number of edges at the VoI and 
   * the cluster coeffecient at the VoI between the two graphs.  */
  val GraphDecision = NonCachingDecision(GraphVoI, List(true, false))

  /* The utility of giving the product is the number of unique visitors minus 
   * 55% of the number of steps in the walk (times 10 just to scale it). The cost is 0 otherwise */
  val util_fcn = (D: Boolean, S: Int, Cov: Int) => if (D) {
    10.0 * (Cov.toDouble - S.toDouble * 0.55)
  } else 0.0
  val GraphUtil = Apply(GraphDecision, NumSteps, RWalkFcn, util_fcn)

  def main(args: Array[String]) {

    println("Running MH Before...")
    val ImpBefore = MetropolisHastings(20000, ProposalScheme.default, 5000, GraphUtil)
    ImpBefore.start()
    val BeforeExp = ImpBefore.computeExpectation(GraphUtil, (t: Double) => t)

    println("Computing Optimal Decision...")
    // Compute custom proposal scheme
    val Scheme = UntypedScheme(() => GraphDecision, Some(ProposalScheme.default))
    val alg = DecisionMetropolisHastings(100000, Scheme, 5000, List(GraphUtil), GraphDecision)
    alg.start()
    alg.setPolicy(GraphDecision)

    println("Running MH After...")
    val ImpAfter = MetropolisHastings(20000, ProposalScheme.default, 5000, GraphUtil)
    ImpAfter.start()
    val AfterExp = ImpAfter.computeExpectation(GraphUtil, (t: Double) => t)

    println("Expected Utility Before Optimization: " + BeforeExp)
    println("Expected Utility After Optimization: " + AfterExp)
    println("Some Optimal Decisions: ")

    val testgraph = makeGraph()
    for { i <- 0 until testgraph.NodeCount } {
      val g1 = testgraph.copy()
      g1.VoI = g1.Nodes(i)
      println("Node " + i + " -> " + GraphDecision.getPolicy(g1) + ", " + GraphDecision.getPolicy().toUtility()(g1))
    }

    alg.kill
    ImpBefore.kill
    ImpAfter.kill

  }

  // An example graph just to see the decisions
  def makeGraph() = {
    val adjmtx = Map(
      0 -> List((0, 8), (0, 7)),
      5 -> List((5, 8), (5, 10)),
      10 -> List((10, 9), (10, 5)),
      1 -> List((1, 8)),
      6 -> List((6, 8), (6, 7)),
      9 -> List((9, 8), (9, 10)),
      2 -> List((2, 3), (2, 8)),
      7 -> List((7, 8), (7, 0), (7, 6), (7, 4)),
      3 -> List((3, 2), (3, 8)),
      8 -> List((8, 9), (8, 1), (8, 6), (8, 7), (8, 5), (8, 3), (8, 4), (8, 2), (8, 0)),
      4 -> List((4, 7), (4, 8)))
    val g = dGraph()
    adjmtx.foreach { m =>
      val n = new Node(m._1)
      m._2.foreach(e => n.insertEdge(new Edge(e._1, e._2)))
      g.Nodes += n.ID -> n
      g.insertEdges(n)
    }
    g
  }

}









