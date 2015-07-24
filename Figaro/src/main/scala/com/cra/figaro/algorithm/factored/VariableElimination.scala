/*
 * VariableElimination.scala
 * Variable elimination algorithm.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection.mutable.{ Map, Set }
import scala.language.postfixOps
import scala.util.control.TailCalls._

/**
 * Trait of algorithms that perform variable elimination.
 *
 * @tparam T The type of entries in the factors.
 */
trait VariableElimination[T] extends FactoredAlgorithm[T] with OneTime {

  /**
   * By default, implementations that inherit this trait have no debug information.
   * Override this if you want a debugging option.
   */
  var debug: Boolean = false

  /**
   * The universe on which this variable elimination algorithm should be applied.
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

  /**
   * Flag indicating whether the run time of each step should be displayed.
   */
  val showTiming: Boolean

  private def optionallyShowTiming[T](op: => T, name: String) =
    if (showTiming) timed(op, name); else op

  /*
  private def expand(): Unit =
    optionallyShowTiming(Expand(universe), "Expansion")
*/

  // The first element of FactorMap is the complete set of factors.
  // The second element maps variables to the factors mentioning that variable.
  // The previous implementation used sets, but that resulted in bugs where an identical factor appeared more than once.
  // The new implementation uses multisets.
  private type FactorMap[T] = Map[Variable[_], MultiSet[Factor[T]]]

  // Add a factor to the list, even if it appears already.
  private def addFactor[T](factor: Factor[T], map: FactorMap[T]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).addOne(factor)))

  // Remove one instance of the factor from the list.
  private def removeFactor[T](factor: Factor[T], map: FactorMap[T]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, HashMultiSet()).removeOne(factor)))

  protected def initialFactorMap(factors: Traversable[Factor[T]]): FactorMap[T] = {
    val map: FactorMap[T] = Map()
    factors foreach (addFactor(_, map))
    map
  }

  protected var recordingFactors: List[Factor[_]] = List()

  /**
   * Some variable elimination algorithms, such as computing the most probable explanation, record values of
   * variables as they are eliminated. Such values are stored in a factor that maps values of the other variables
   * to a value of the eliminated variable. This factor is produced by finding the value of the variable that
   * "maximizes" the entry associated with the value in the product factor resulting from eliminating this
   * variable, for some maximization function. The recordingFunction determines which of two entries is greater
   * according to the maximization function. It returns true iff the second entry is greater. The recording
   * function is an option so that variable elimination algorithms that do not use it can ignore it.
   */
  val comparator: Option[(T, T) => Boolean] = None

  private def eliminate(
    variable: Variable[_],
    factors: MultiSet[Factor[T]],
    map: FactorMap[T]): Unit = {
    val varFactors = map(variable)
    if (debug) {
      println("*****************\nEliminating " + variable.id)
      println("Input factors:")
      for { factor <- varFactors } { println(factor.toReadableString) }
    }
    if (varFactors nonEmpty) {
      val productFactor = varFactors reduceLeft (_.product(_))
      val resultFactor = productFactor.sumOver(variable)
      if (debug) println("Result factor\n" + resultFactor.toReadableString)
      comparator match {
        case None => ()
        case Some(recorder) => recordingFactors ::= productFactor.recordArgMax(variable, recorder)
      }
      varFactors.foreach(factors.removeOne(_))
      factors.addOne(resultFactor)
      varFactors.foreach(removeFactor(_, map))
      map -= variable
      addFactor(resultFactor, map)
    }
  }

  // Wraps the TailRec class and returns the result
  protected def eliminateInOrder(
    order: List[Variable[_]],
    factors: MultiSet[Factor[T]],
    map: FactorMap[T]): MultiSet[Factor[T]] = {
    callEliminateInOrder(order, factors, map).result
  }

  /*
   *  TailRec class turns a tail-recursive method into a while loop
   *  The result needs to be extracted explicitly
   */
  private def callEliminateInOrder(
    order: List[Variable[_]],
    factors: MultiSet[Factor[T]],
    map: FactorMap[T]): TailRec[MultiSet[Factor[T]]] = {
    order match {
      case Nil =>
        done(factors)
      case first :: rest =>
        eliminate(first, factors, map)
        tailcall(callEliminateInOrder(rest, factors, map))
    }
  }
  
  
  private[figaro] def ve(): Unit = {
    //expand()
    val (neededElements, _) = getNeededElements(starterElements, Int.MaxValue)
    val allFactors = optionallyShowTiming(getFactors(neededElements, targetElements), "Getting factors")
    val targetVariables = targetElements.map(Variable(_))
    doElimination(allFactors, targetVariables)
  }

  protected def doElimination(allFactors: List[Factor[T]], targetVariables: Seq[Variable[_]]) {
    recordingFactors = List()
    if (debug) {
      println("*****************\nStarting factors\n")
      allFactors.foreach((f: Factor[_]) => println(f.toReadableString))
    }
    val (_, order) = optionallyShowTiming(VariableElimination.eliminationOrder(allFactors, targetVariables), "Computing elimination order")
    val factorsAfterElimination =
      optionallyShowTiming(eliminateInOrder(order, HashMultiSet(allFactors: _*), initialFactorMap(allFactors)), "Elimination")
    if (debug) println("*****************")
    if (debug) factorsAfterElimination foreach (f => println(f.toReadableString))
    optionallyShowTiming(finish(factorsAfterElimination, order), "Finalizing")
    if (debug) targetFactors.values foreach (f => println(f.toReadableString))
  }

  protected[figaro] var targetFactors: Map[Element[_], Factor[T]] = Map()

  /**
   * All implementation of variable elimination must specify what to do after variables have been eliminated.
   */
  def finish(factorsAfterElimination: MultiSet[Factor[T]], eliminationOrder: List[Variable[_]]): Unit

  def run() = ve()

}

/**
 * Variable elimination over probabilistic factors.
 */
trait ProbabilisticVariableElimination extends VariableElimination[Double] {
  def getFactors(allElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[Double]] = {
    if (debug) {
      println("Elements appearing in factors and their ranges:")
      for { element <- allElements } {
        println(Variable(element).id + "(" + element.name.string + "@" + element.hashCode + ")" + ": " + element + ": " + Variable(element).range.mkString(","))
      }
    }
    Factory.removeFactors()
    val thisUniverseFactors = allElements flatMap (Factory.make(_))
    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield Factory.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))
    dependentUniverseFactors ::: thisUniverseFactors
  }

}

/**
 * Variable elimination algorithm that computes the conditional probability of query elements.
 *
 */
class ProbQueryVariableElimination(override val universe: Universe, targets: Element[_]*)(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends OneTimeProbQuery
  with ProbabilisticVariableElimination {
  val targetElements = targets.toList
  lazy val queryTargets = targets.toList

  val semiring = SumProductSemiring()

  private def marginalizeToTarget(factor: Factor[Double], target: Element[_]): Unit = {
    val unnormalizedTargetFactor = factor.marginalizeTo(semiring.asInstanceOf[Semiring[Double]], Variable(target))
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, _ + _)
    //val targetFactor = Factory.make[Double](unnormalizedTargetFactor.variables)
    val targetFactor = unnormalizedTargetFactor.mapTo((d: Double) => d / z)
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[Double]) =
    targets foreach (marginalizeToTarget(resultFactor, _))

  private def makeResultFactor(factorsAfterElimination: MultiSet[Factor[Double]]): Factor[Double] = {
    // It is possible that there are no factors (this will happen if there are  no queries or evidence).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    factorsAfterElimination.foldLeft(Factory.unit(semiring))(_.product(_))
  }

  def finish(factorsAfterElimination: MultiSet[Factor[Double]], eliminationOrder: List[Variable[_]]) =
    marginalize(makeResultFactor(factorsAfterElimination))

  /**
   * Computes the normalized distribution over a single target element.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    if (factor.numVars > 1) throw new UnsupportedAlgorithmException(target)
    val targetVar = if (factor.output.nonEmpty) factor.output.head.asInstanceOf[Variable[T]] else factor.parents.head.asInstanceOf[Variable[T]]
    val dist = factor.getIndices.filter(f => targetVar.range(f.head).isRegular).map(f => (factor.get(f), targetVar.range(f.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  /**
   * Computes the expectation of a given function for single target element.
   */
  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }
}

object VariableElimination {
  /**
   * Method for choosing the elimination order.
   * The default order chooses first the variable that
   * minimizes the number of extra factor entries that would be created when it is eliminated.
   * Override this method if you want a different rule.
   *
   * Returns the score of the ordering as well as the ordering.
   */
  def eliminationOrder[T](factors: Traversable[Factor[T]], toPreserve: Traversable[Variable[_]]): (Double, List[Variable[_]]) = {
    val eliminableVars = (Set[Variable[_]]() /: factors)(_ ++ _.variables) -- toPreserve
    var initialGraph = new VEGraph(factors)
    val candidates = new HeapPriorityMap[Variable[_], Double]
    eliminableVars foreach (v => candidates += v -> initialGraph.score(v))
    eliminationOrderHelper(candidates, toPreserve, initialGraph, Double.NegativeInfinity, List())
  }

  @tailrec private def eliminationOrderHelper(candidates: PriorityMap[Variable[_], Double],
    toPreserve: Traversable[Variable[_]],
    graph: VEGraph,
    currentScore: Double,
    accum: List[Variable[_]]): (Double, List[Variable[_]]) = {
    if (candidates.isEmpty) (currentScore, accum.reverse)
    else {
      val (best, bestScore) = candidates.extractMin()
      // do not read the best variable after it has been removed, and do not add the preserved variables
      val touched = graph.info(best).neighbors - best -- toPreserve
      val nextGraph = graph.eliminate(best)
      touched foreach (v => candidates += v -> graph.score(v))
      eliminationOrderHelper(candidates, toPreserve, nextGraph, bestScore max currentScore, best :: accum)
    }
  }

  /**
   * Create a variable elimination computer with the given target query variables in the current default
   * universe.
   */
  def apply(targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryVariableElimination(universe, targets: _*)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a variable elimination computer with the given target query variables in the current default
   * universe, with debug information enabled.
   */
  def debugged(targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryVariableElimination(universe, targets: _*)(
      true,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) { debug = true }
  /**
   * Create a variable elimination computer with the given target query variables in the current default
   * universe, with timing information enabled.
   */
  def timed(targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryVariableElimination(universe, targets: _*)(
      true,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a variable elimination computer with the given target query variables and using the given
   * dependent universes in the current default universe.
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])], targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryVariableElimination(universe, targets: _*)(
      false,
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
   * Create a variable elimination computer with the given target query variables and using the given
   * dependent universes in the current default universe. Use the given dependent algorithm function to
   * determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply(
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryVariableElimination(universe, targets: _*)(
      false,
      dependentUniverses,
      dependentAlgorithm)

  /**
   * Use VE to compute the probability that the given element satisfies the given predicate.
   */
  def probability[T](target: Element[T], predicate: T => Boolean): Double = {
    val alg = VariableElimination(target)
    alg.start()
    val result = alg.probability(target, predicate)
    alg.kill()
    result
  }

  /**
   * Use VE to compute the probability that the given element has the given value.
   */
  def probability[T](target: Element[T], value: T): Double =
    probability(target, (t: T) => t == value)
}
