/*
 * BeliefPropagation.scala  
 * A belief propagation algorithm.
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

import scala.Option.option2Iterable

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import com.cra.figaro.algorithm.OneTimeProbQuery
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.Variable
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language.Element
import com.cra.figaro.language.Universe

/**
 * Objects for performing belief propagation. T is the type of entries in the factors.
 */
trait BeliefPropagation[T] extends FactoredAlgorithm[T] with OneTime {

  /**
   * By default, implementations that inherit this trait have no debug information.
   * Override this if you want a debugging option.
   */
  val debug: Boolean = false

  /**
   * The universe on which this belief propagation algorithm should be applied.
   */
  val universe: Universe

  protected[figaro] val factorGraph: FactorGraph[T]

  /**
   * Returns a new message from `source` to `target`.
   */
  protected[figaro] def newMessage(source: Node, target: Node): Factor[T] = {
    val message: Factor[T] = (source, target) match {
      case (f: FactorNode, v: VariableNode) => getNewMessageFactorToVar(f, v)
      case (v: VariableNode, f: FactorNode) => getNewMessageVarToFactor(v, f)
      case _ => throw new UnsupportedOperationException()
    }

    if (debug) {
      println("message: " + source + " to " + target)
      println(message.toReadableString)
    }
    message
  }

  /*
   * A message from a factor Node u to a variable Node v is the product of the factor with
   * messages from all other Nodes, marginalized over all variables except xv:
   */
  private def getNewMessageFactorToVar(fn: FactorNode, vn: VariableNode) = {
    val initFactor = factorGraph.getFactorForNode(fn)
    val neighborList = factorGraph.getNeighbors(fn, vn)
    val messageList = neighborList map (factorGraph.getLastMessage(_, fn))

    if (messageList.isEmpty) {
      initFactor.marginalizeTo(vn.variable, semiring.sum, semiring.zero)
    } else {
      val total = messageList.reduceLeft(_.product(_, semiring.product))
      initFactor.product(total, semiring.product).marginalizeTo(vn.variable, semiring.sum, semiring.zero)
    }
  }

  /*
   * A message from a variable Node v to a factor Node u is the product of the messages from
   * all other neighboring factor Nodes (except the recipient; alternatively one can say the
   * recipient sends the message "1"):
   */
  private def getNewMessageVarToFactor(vn: VariableNode, fn: FactorNode) = {
    val neighborList = factorGraph.getNeighbors(vn, fn)
    val messageList = neighborList map (factorGraph.getLastMessage(_, vn))

    if (messageList.isEmpty) factorGraph.uniformFactor(List(vn.variable))
    else messageList.reduceLeft(_.product(_, semiring.product))
  }

  /**
   * Returns the product of all messages from `source`'s neighbors to `source`.
   */
  def belief(source: Node) = {
    val messageList = factorGraph.getNeighbors(source) map (factorGraph.getLastMessage(_, source))

    if (messageList.isEmpty) {
      source match {
        case fn: FactorNode => factorGraph.uniformFactor(fn.variables)
        case vn: VariableNode => factorGraph.uniformFactor(List(vn.variable))
      }
    } else messageList.reduceLeft(_.product(_, semiring.product))

  }

  // This is not really correct. Need some way of updating factorGraph in the trait
  private def asynchronousUpdate(): Unit = {
    factorGraph.getNodes.foreach { node1 =>
      factorGraph.getNeighbors(node1).foreach { node2 =>
        factorGraph.update(node1, node2, newMessage(node1, node2))
      }
    }
  }

  /*
   * Propagates one set of messages.
   */
  private def synchronousUpdate(): Unit = {
    val updates = factorGraph.getNodes.flatMap { node1 =>
      factorGraph.getNeighbors(node1).map { node2 =>
        (node1, node2, newMessage(node1, node2))
      }
    }
    updates.foreach { u => factorGraph.update(u._1, u._2, u._3) }
  }

  /**
   * Runs this belief propagation algorithm for the specified number of iterations. An iteration
   * consists of each node of the factor graph sending a message to each of its neighbors.
   */
  def run(iterations: Int) {
    if (debug) {
      println("Factor graph: ")
      println(factorGraph.getNodes.map(n => n -> factorGraph.getNeighbors(n)).toMap.mkString("\n"))
      println()
    }

    for (i <- 1 to iterations) {
      if (debug)
        println("Iteration " + i + ": ")

      synchronousUpdate()
    }
  }

}

trait ProbabilisticBeliefPropagation extends BeliefPropagation[Double] {

  def normalize(factor: Factor[Double]): Factor[Double] = {
    val z = factor.foldLeft(semiring.zero, _ + _)
    val normedFactor = new Factor[Double](factor.variables)
    factor.mapTo((d: Double) => d / z, normedFactor)
    normedFactor
  }

  override def newMessage(source: Node, target: Node): Factor[Double] = {
    val newMessage = super.newMessage(source, target)
    normalize(newMessage)
  }

  def getFactors(targetVariables: Seq[Variable[_]]): List[Factor[Double]] = {
    val allElements = universe.activeElements
    val thisUniverseFactors = allElements flatMap (ProbFactor.make(_))
    if (debug) {
      println("Element ids:")
      for { element <- universe.activeElements } { println(Variable(element).id + "(" + element.name.string + ")" + ": " + element) }
    }
    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield ProbFactor.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))
    dependentUniverseFactors ::: thisUniverseFactors
  }
}

class ProbQueryBeliefPropagation(val iterations: Int, override val universe: Universe)(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends OneTimeProbQuery
  with ProbabilisticBeliefPropagation {

  val queryTargets = universe.activeElements

  val semiring = SumProductSemiring

  val factorGraph = new BasicFactorGraph(getFactors(Seq()), semiring.product)

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val targetVar = Variable(target)
    val targetNode = factorGraph.getNodes.find { node =>
      node match {
        case vn: VariableNode => vn.variable == targetVar
        case _ => false
      }
    }

    if (targetNode.isEmpty) {
      Stream[(Double, T)]()
    } else {
      val factor = normalize(belief(targetNode.get))
      val dist = targetVar.range.zipWithIndex.map(pair => (factor.get(List(pair._2)), pair._1))
      dist.toStream
    }
  }

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }

  def run(): Unit = {
    if (debug) {
      println("Element ids:")
      for { element <- universe.activeElements } { println(Variable(element).id + "(" + element.name.string + ")" + ": " + element) }
    }
    run(iterations)
  }

}

object BeliefPropagation {
  /**
   * Creates a belief propagation computer in the current default universe.
   */
  def apply(iterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(iterations, universe)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a belief propagation computer with the given target query variables in the current default
   * universe, with debug information enabled.
   */
  def debugged(iterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(iterations, universe)(
      true,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) { override val debug = true }
  /**
   * Create a belief propagation computer with the given target query variables in the current default
   * universe, with timing information enabled.
   */
  def timed(iterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(iterations, universe)(
      true,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a belief propagation computer with the given target query variables and using the given
   * dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], iterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(iterations, universe)(
      false,
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a belief propagation computer with the given target query variables and using the given
   * dependent universes in the current default universe. Use the given dependent algorithm function to
   * determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    iterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(iterations, universe)(
      false,
      dependentUniverses,
      dependentAlgorithm)

}



