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
import com.cra.figaro.algorithm.lazyfactored.LazyValues

/**
 * Trait for performing belief propagation.
 * 
 * @tparam T The type of entries in the factors.
 */
trait BeliefPropagation[T] extends FactoredAlgorithm[T] {

  /**
   * By default, implementations that inherit this trait have no debug information.
   * Override this if you want a debugging option.
   */
  val debug: Boolean = false

  /**
   * The universe on which this belief propagation algorithm should be applied.
   */
  val universe: Universe

  /* The factor graph for this BP object */
  protected[figaro] val factorGraph: FactorGraph[T]

  /*
   * Returns a new message from a source node to a target node.
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
   * A message from a factor Node to a variable Node is the product of the factor with
   * messages from all other Nodes (except the destination node), 
   * marginalized over all variables except the variable:
   */
  private def getNewMessageFactorToVar(fn: FactorNode, vn: VariableNode) = {
    val initFactor = factorGraph.getFactorForNode(fn)
    val neighborList = factorGraph.getNeighbors(fn, vn)
    val messageList = neighborList map (factorGraph.getLastMessage(_, fn))

    if (messageList.isEmpty) {
      initFactor.marginalizeTo(vn.variable, semiring)
    } else {
      val total = messageList.reduceLeft(_.product(_, semiring))
      initFactor.product(total, semiring).marginalizeTo(vn.variable, semiring)
    }
  }

  /*
   * A message from a variable Node to a factor Node is the product of the messages from
   * all other neighboring factor Nodes (except the recipient; alternatively one can say the
   * recipient sends the message "1"):
   */
  private def getNewMessageVarToFactor(vn: VariableNode, fn: FactorNode) = {
    val neighborList = factorGraph.getNeighbors(vn, fn)
    val messageList = neighborList map (factorGraph.getLastMessage(_, vn))

    if (messageList.isEmpty) factorGraph.uniformFactor(List(vn.variable))
    else messageList.reduceLeft(_.product(_, semiring))
  }

  /**
   * Returns the product of all messages from a source node's neighbors to itself.
   */
  def belief(source: Node) = {
    val messageList = factorGraph.getNeighbors(source) map (factorGraph.getLastMessage(_, source))

    if (messageList.isEmpty) {
      source match {
        case fn: FactorNode => factorGraph.uniformFactor(fn.variables)
        case vn: VariableNode => factorGraph.uniformFactor(List(vn.variable))
      }
    } else messageList.reduceLeft(_.product(_, semiring))

  }

  /*
   * This is intended to perform an asynchronous update of the factor graph.
   * It is unclear if this is the correct implementation since messages
   * are updating in the factor graph immediately
   */
  private def asynchronousUpdate(): Unit = {
    factorGraph.getNodes.foreach { node1 =>
      factorGraph.getNeighbors(node1).foreach { node2 =>
        factorGraph.update(node1, node2, newMessage(node1, node2))
      }
    }
  }

  /*
   * Propagates one set of synchronous message in the graph
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
   * Runs this belief propagation algorithm for one iteration. An iteration
   * consists of each node of the factor graph sending a message to each of its neighbors.
   */
  def runStep() {
    if (debug) {
      println("Factor graph: ")
      println(factorGraph.getNodes.map(n => n -> factorGraph.getNeighbors(n)).toMap.mkString("\n"))
      println()
    }
    synchronousUpdate()
  }

}

/**
 * Trait for probabilistic BP algorithms
 */
trait ProbabilisticBeliefPropagation extends BeliefPropagation[Double] {

  /** 
   *  Normalize a factor
   */
  def normalize(factor: Factor[Double]): Factor[Double] = {
    val z = factor.foldLeft(semiring.zero, _ + _)
    val normedFactor = new Factor[Double](factor.variables)
    factor.mapTo((d: Double) => d / z, normedFactor)
    normedFactor
  }

  /*
   * Overrides newMessage in the BP with normalization at the end
   */
  override protected[figaro] def newMessage(source: Node, target: Node): Factor[Double] = {
    val newMessage = super.newMessage(source, target)
    normalize(newMessage)
  }

  /**
   * Returns the factors needed for BP. Since BP operates on a complete factor graph, factors are created
   * for all elements in the universe. 
   */
  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upperBounds: Boolean = false): List[Factor[Double]] = {
    val allElements = universe.activeElements
    
    // Due to Lazy values change, have to force expansion of all elements
    LazyValues(universe).expandAll(allElements.toSet.map((elem: Element[_]) => (elem, Integer.MAX_VALUE)))
    
    val thisUniverseFactors = allElements flatMap (ProbFactor.make(_))
    if (debug) {
      println("Element ids:")
      for { element <- universe.activeElements } { println(Variable(element).id + "(" + element.name.string + ")" + ": " + element) }
    }
    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield ProbFactor.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))
    dependentUniverseFactors ::: thisUniverseFactors
  }

  /**
   * Get the belief for an element 
   */
  def getBeliefsForElement[T](target: Element[T]): List[(Double, T)] = {
    val targetVar = Variable(target)
    val targetNode = factorGraph.getNodes.find { node =>
      node match {
        case vn: VariableNode => vn.variable == targetVar
        case _ => false
      }
    }
    if (targetNode.isEmpty) {
      List[(Double, T)]()
    } else {
      val factor = normalize(belief(targetNode.get))
      targetVar.range.zipWithIndex.map(pair => (factor.get(List(pair._2)), pair._1.value))
    }
  }

}

/**
 * Trait for One Time BP algorithms
 */
trait OneTimeProbabilisticBeliefPropagation extends ProbabilisticBeliefPropagation with OneTime {
  val iterations: Int

  def run() = {
    if (debug) {
      println("Element ids:")
      for { element <- universe.activeElements } { println(Variable(element).id + "(" + element.name.string + ")" + ": " + element) }
    }
    for { i <- 1 to iterations } { runStep() }
  }
}

/**
 * Trait for Anytime BP algorithms
 */
trait AnytimeProbabilisticBeliefPropagation extends ProbabilisticBeliefPropagation with Anytime

/**
 * Class to implement a probability query BP algorithm 
 */
abstract class ProbQueryBeliefPropagation(override val universe: Universe)(
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends ProbQueryAlgorithm
  with ProbabilisticBeliefPropagation {

  val queryTargets = universe.activeElements

  val semiring = SumProductSemiring

  val factorGraph = new BasicFactorGraph(getFactors(List(), List()), semiring)

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = getBeliefsForElement(target).toStream

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }
}


object BeliefPropagation {
  /**
   * Creates a One Time belief propagation computer in the current default universe.
   */
  def apply(myIterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) 
      with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations }

  /**
   * Creates a Anytime belief propagation computer in the current default universe.
   */
  def apply()(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) 
      with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery

  /**
   * Create a One Time belief propagation computer current default universe, with debug information enabled.
   */
  def debugged(myIterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) 
      with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations; override val debug = true }

  /**
   * Create a Anytime belief propagation computer using the given dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], myIterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations }

  /**
   * Create a One Time belief propagation computer using the given dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])])(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery

  /**
   * Create a One Time belief propagation computer using the given dependent universes in the current 
   * default universe. Use the given dependent algorithm function to determine the algorithm to use 
   * to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    myIterations: Int)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      dependentUniverses,
      dependentAlgorithm) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations }

  /**
   * Create a Anytime belief propagation computer using the given dependent universes in the current 
   * default universe. Use the given dependent algorithm function to determine the algorithm to use 
   * to compute probability of evidence in each dependent universe.
   */
 def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe)(
      dependentUniverses,
      dependentAlgorithm) with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery

}



