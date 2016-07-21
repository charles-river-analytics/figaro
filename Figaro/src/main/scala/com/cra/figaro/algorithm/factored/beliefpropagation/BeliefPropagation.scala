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
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language._
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.lazyfactored.BoundedProbFactor
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.factors.factory.Factory

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
   * Since BP uses division to compute messages, the semiring has to have a division function defined
   * and must be log convertable.
   * Note that BP operates in log space and any semiring must be log convertible
   * If you define a non-log semiring, it will automatically convert, and convert it back to normal space at the end
   * If you define a log semiring, it won't convert to log or convert from log.
   * In other words, it outputs the answer in the space specified by the semiring
   */
  override val semiring: LogConvertibleSemiRing[T]

  /**
   * Returns the log space version of the semiring (or the semiring if already in log space)
   */
  protected def logSpaceSemiring(): LogConvertibleSemiRing[T] = if (semiring.isLog) semiring else semiring.convert

  /**
   * Elements towards which queries are directed. By default, these are the target elements.
   * This is overridden by DecisionVariableElimination, where it also includes utility variables.
   */
  def starterElements: List[Element[_]] = targetElements

  /* The factor graph for this BP object */
  protected[figaro] var factorGraph: FactorGraph[T] = _

  /* The beliefs associated with each node in the factor graph. The belief is the product 
   * of all messages to the node times any factor at the node
   */
  private[figaro] val beliefMap: Map[Node, Factor[T]] = Map()

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
  protected def getNewMessageFactorToVar(fn: FactorNode, vn: VariableNode) = {
    val vnFactor = factorGraph.getLastMessage(vn, fn)

    val total = beliefMap(fn).combination(vnFactor, logSpaceSemiring().divide)
    total.marginalizeTo(vn.variable)
  }

  /*
   * A message from a variable Node to a factor Node is the product of the messages from
   * all other neighboring factor Nodes (except the recipient; alternatively one can say the
   * recipient sends the message "1"):
   */
  protected def getNewMessageVarToFactor(vn: VariableNode, fn: FactorNode) = {
    val fnFactor = factorGraph.getLastMessage(fn, vn)

    val total = beliefMap(vn).combination(fnFactor, logSpaceSemiring().divide)
    total
  }

  /**
   * Returns the product of all messages from a source node's neighbors to itself.
   */
  def belief(source: Node) = {
    val messageList = factorGraph.getNeighbors(source) map (factorGraph.getLastMessage(_, source))

    if (messageList.isEmpty) {
      source match {
        case fn: FactorNode => factorGraph.getFactorForNode(fn) //factorGraph.uniformFactor(fn.variables.toList)
        case vn: VariableNode => factorGraph.uniformFactor(List(vn.variable))
      }
    } else {
      val messageBelief = messageList.reduceLeft(_.product(_))
      source match {
        case fn: FactorNode => messageBelief.product(factorGraph.getFactorForNode(fn))
        case vn: VariableNode => messageBelief
      }
    }

  }

  /*
   * Propagates one set of synchronous message in the graph
   */
  private def synchronousUpdate(): Unit = {
    val updates = factorGraph.getNodes.par.flatMap { node1 =>
      factorGraph.getNeighbors(node1).map { node2 =>
        (node1, node2, newMessage(node1, node2))
      }
    }
    updates.foreach { u => factorGraph.update(u._1, u._2, u._3) }
    // Update the beliefs of each node
    factorGraph.getNodes.foreach(n => beliefMap.update(n, belief(n)))
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
    if (debug) {
      beliefMap.foreach(a => println(a._1 + " => " + a._2)); println
      println("Factor Messages:")
      factorGraph.getNodes.foreach { n =>
        println(n + ": ")
        println(factorGraph.getMessagesForNode(n))
      }
    }
  }

  override def initialize() = {
    factorGraph.getNodes.foreach(n => beliefMap.update(n, belief(n)))
  }

}

/**
 * Trait for probabilistic BP algorithms.
 */
trait ProbabilisticBeliefPropagation extends BeliefPropagation[Double] {

  /**
   *  Normalize a factor.
   */
  def normalize(factor: Factor[Double]): Factor[Double] = {
    val z = logSpaceSemiring().sumMany(factor.contents.values)
    // Since we're in log space, d - z = log(exp(d)/exp(z))
    factor.mapTo((d: Double) => if (z != logSpaceSemiring().zero) d - z else logSpaceSemiring().zero)
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
    val thisUniverseFactors = (neededElements flatMap (Factory.makeFactorsForElement(_, upperBounds, true))).filterNot(_.isEmpty)
    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield Factory.makeDependentFactor(Variable.cc, universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))
    val factors = dependentUniverseFactors ::: thisUniverseFactors
    // To prevent underflow, we do all computation in log space    
    convertFactors(factors)
  }

  /*
   * Convert factors to log space if necessary
   */
  protected def convertFactors(factors: List[Factor[Double]]) = {
    if (factors.size == 0) {
      factors
    } else {
      if (factors.head.semiring == semiring && semiring.isLog == false) {
        // semiring in factors matches semiring for BP, and not in log space so need to convert
        factors.map(makeLogarithmic(_))
      } else if (factors.head.semiring == semiring && semiring.isLog == true) {
        // semiring in factors matches semiring for BP, and already in log space so no need to convert
        factors
      } else if (factors.head.semiring == semiring.convert() && semiring.isLog == false) {
        // semiring in factors matches the converted semiring, and not in log space. But the factors are already in log space so no need to convert
        factors
      } else if (factors.head.semiring == semiring.convert() && semiring.isLog == true) {
        // semiring in factors matches the converted semiring, and in log space. Need to convert
        factors.map(makeLogarithmic(_))
      } else {
        // Incompatible case. This might cause a problem if they are not converted eventually
        factors.map(makeLogarithmic(_))
      }
    }
  }

  private[figaro] def makeLogarithmic(factor: Factor[Double]): Factor[Double] = {
    factor.mapTo((d: Double) => Math.log(d), logSpaceSemiring())
  }

  private[figaro] def unmakeLogarithmic(factor: Factor[Double]): Factor[Double] = {
    factor.mapTo((d: Double) => Math.exp(d), logSpaceSemiring().convert)
  }

  /**
   * Get the belief for an element.
   */
  protected[figaro] def getBeliefsForElement[T](target: Element[T]): List[(Double, T)] = {
    val finalFactor = getFinalFactorForElement(target)
    if (finalFactor.isEmpty) {
      List[(Double, T)]()
    } else {
      factorToBeliefs(finalFactor).asInstanceOf[List[(Double, T)]]
    }
  }

  /*
   * Find the node in the factor graph corresponding to a particular element
   */
  protected[figaro] def findNodeForElement[T](target: Element[T]): Node = {
    val targetNode = factorGraph.getNodes.find { node =>
      node match {
        case vn: VariableNode => {
          vn.variable match {
            case elemVar: ElementVariable[_] => elemVar.element == target
            case _ => false
          }
        }
        case _ => false
      }
    }
    targetNode.get
  }

  /*
   * Convert a factor of a single variable to beliefs
   * Creates and exception if the factor has more than one variable
   */
  protected[figaro] def factorToBeliefs[T](factor: Factor[Double]): List[(Double, _)] = {
    if (factor.numVars > 1) throw new IllegalArgumentException

    val variable = factor.variables(0)
    val ff = normalize(factor)
    val inOriginalSpace = semiring.isLog match {
      case true => ff
      case false => unmakeLogarithmic(ff)
    }
    inOriginalSpace.getIndices.filter(f => variable.range(f.head).isRegular).map(f => (inOriginalSpace.get(f), variable.range(f.head).value)).toList
  }

  /**
   * Get the final factor for an element.
   */
  def getFinalFactorForElement[T](target: Element[T]): Factor[Double] = {
    beliefMap(findNodeForElement(target))
  }

}

/**
 * Trait for One Time BP algorithms.
 */
trait OneTimeProbabilisticBeliefPropagation extends ProbabilisticBeliefPropagation with OneTime {
  def iterations: Int
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
 * Trait for Anytime BP algorithms.
 */
trait AnytimeProbabilisticBeliefPropagation extends ProbabilisticBeliefPropagation with Anytime

/**
 * Class to implement a probability query BP algorithm.
 */
abstract class ProbQueryBeliefPropagation(override val universe: Universe, targets: Element[_]*)(
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
  depth: Int = Int.MaxValue, upperBounds: Boolean = false)
  extends ProbQueryAlgorithm
  with ProbabilisticBeliefPropagation {

  val targetElements = targets.toList

  val queryTargets = targetElements

  val semiring = SumProductSemiring()

  var neededElements: List[Element[_]] = _
  var needsBounds: Boolean = _

  def generateGraph() = {
    val needs = getNeededElements(starterElements, depth)
    neededElements = needs._1
    needsBounds = needs._2

    // Depth < MaxValue implies we are using bounds  
    val factors = if (depth < Int.MaxValue && needsBounds) {
      getFactors(neededElements, targetElements, upperBounds)
    } else {
      getFactors(neededElements, targetElements)
    }

    factorGraph = new BasicFactorGraph(factors, logSpaceSemiring()): FactorGraph[Double]
  }

  override def initialize() = {
    if (factorGraph == null) generateGraph()
    super.initialize
  }

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
   * Use BP to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    val alg = BeliefPropagation(10, target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use BP to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T): Double =
    probability(target, (t: T) => t == value)

  /**
   * Lazy version of BP that operates only on bounds.
   */
  def lazyBP(myIterations: Int, depth: Int, upperBounds: Boolean, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryBeliefPropagation(universe, targets: _*)(
      List(), (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u),
      depth, upperBounds) with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { val iterations = myIterations; override val debug = false }

}



