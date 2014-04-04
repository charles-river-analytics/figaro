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
import com.cra.figaro.algorithm.lazyfactored.BoundedProbFactor

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

  /**
   * Target elements that should not be eliminated but should be available for querying.
   */
  val targetElements: List[Element[_]]

  /**
   * Elements towards which queries are directed. By default, these are the target elements.
   * This is overridden by DecisionVariableElimination, where it also includes utility variables.
   */
  def starterElements: List[Element[_]] = targetElements
  
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
      initFactor.marginalizeTo(semiring, vn.variable)
    } else {
      val total = messageList.reduceLeft(_.product(_, semiring))
      initFactor.product(total, semiring).marginalizeTo(semiring, vn.variable)
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
    //val z = factor.foldLeft(semiring.zero, _ + _)
    val z = semiring.sumMany(factor.contents.values)
    val normedFactor = new Factor[Double](factor.variables)
    // Since we're in log space, d - z = log(exp(d)/exp(z))
    factor.mapTo((d: Double) => if (z != semiring.zero) d - z else semiring.zero, normedFactor)
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

    ProbFactor.removeFactors()
    //val thisUniverseFactors = neededElements flatMap (ProbFactor.make(_))
    val thisUniverseFactors = (neededElements flatMap (BoundedProbFactor.make(_, upperBounds))).filterNot(_.isEmpty)
    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield ProbFactor.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))
    val factors = dependentUniverseFactors ::: thisUniverseFactors
    // To prevent underflow, we do all computation in log space
    factors.map(makeLogarithmic(_))
  }

  private def makeLogarithmic(factor: Factor[Double]): Factor[Double] = {
    val result = new Factor[Double](factor.variables)
    factor.mapTo((d: Double) => Math.log(d), result)
    result
  }
  /**
   * Get the belief for an element
   */
  protected[figaro] def getBeliefsForElement[T](target: Element[T]): List[(Double, T)] = {
    val finalFactor = getFinalFactorForElement(target)
    if (finalFactor.isEmpty) {
      List[(Double, T)]()
    } else {
      val factor = normalize(finalFactor)
      val factorVariable = Variable(target)
      // Since all computations have been in log space, we get out of log space here to provide the final beliefs
      factorVariable.range.zipWithIndex.map(pair => (Math.exp(factor.get(List(pair._2))), pair._1.value))
    }
  }
  
  /**
   * Get the final factor for an element
   */
  def getFinalFactorForElement[T](target: Element[T]): Factor[Double] = {
    val targetVar = Variable(target)
    val targetNode = factorGraph.getNodes.find { node =>
      node match {
        case vn: VariableNode => vn.variable == targetVar
        case _ => false
      }
    }
    if (targetNode.isEmpty) {
      new Factor[Double](List())
    } else {
      belief(targetNode.get)
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
      val varNodes = factorGraph.getNodes.filter(_.isInstanceOf[VariableNode])
      val allVars = (Set[Variable[_]]() /: factorGraph.getNodes)((s: Set[Variable[_]], n: Node) => {
        val a = (n match {
          case vn: VariableNode => Set(vn.variable)
          case fn: FactorNode => fn.variables
        })
        s ++ a
      })
      println("*****************\nElement ids:")
      for { variable <- allVars } {
        variable match {
          case elemVar: /*Extended*/ ElementVariable[_] =>
            println(variable.id + "(" + elemVar.element.name.string + ")" + "@" + elemVar.element.hashCode + ": " + elemVar.element)
          case _ =>
            println(variable.id + ": not an element variable")
        }
      }
      println("*****************\nOriginal Factors:")
      factorGraph.getNodes.foreach { n =>
        n match {
          case fn: FactorNode => println(factorGraph.getFactorForNode(fn).toReadableString)
          case _ => 
        }  
      }
      println("*****************")
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
abstract class ProbQueryBeliefPropagation(override val universe: Universe, targets: Element[_]*)(
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
  depth: Int = Int.MaxValue, upperBounds: Boolean = false)
  extends ProbQueryAlgorithm
  with ProbabilisticBeliefPropagation {

  val targetElements = targets.toList

  val queryTargets = targetElements

  val semiring = LogSumProductSemiring

  val (neededElements, needsBounds) = getNeededElements(starterElements, depth)

  // Depth < MaxValue implies we are using bounds  
  val factors = if (depth < Int.MaxValue && needsBounds) {
    getFactors(neededElements, targetElements, upperBounds)
  } else {
    getFactors(neededElements, targetElements)
  }

  val factorGraph = new BasicFactorGraph(factors, semiring)

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = getBeliefsForElement(target).toStream

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }
}

object BeliefPropagation {
  /**
   * Creates a One Time belief propagation computer in the current default universe.
   */
  def apply(myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations }

  /**
   * Creates a Anytime belief propagation computer in the current default universe.
   */
  def apply(targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery

  /**
   * Create a One Time belief propagation computer current default universe, with debug information enabled.
   */
  def debugged(myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations; override val debug = true }

  /**
   * Create a Anytime belief propagation computer using the given dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations }

  /**
   * Create a One Time belief propagation computer using the given dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery

  /**
   * Create a One Time belief propagation computer using the given dependent universes in the current
   * default universe. Use the given dependent algorithm function to determine the algorithm to use
   * to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    myIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      dependentUniverses,
      dependentAlgorithm) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations }

  /**
   * Create a Anytime belief propagation computer using the given dependent universes in the current
   * default universe. Use the given dependent algorithm function to determine the algorithm to use
   * to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      dependentUniverses,
      dependentAlgorithm) with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery

  /**
   * Lazy version of BP that operates only on bounds
   */
  def lazyBP(myIterations: Int, depth: Int, upperBounds: Boolean, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      List(), (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
      depth, upperBounds) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations; override val debug = false }

}



