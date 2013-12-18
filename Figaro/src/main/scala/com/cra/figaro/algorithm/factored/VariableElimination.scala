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
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection.mutable.{ Map, Set }
import scala.language.postfixOps

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
  val debug: Boolean = false

  /**
   * The universe on which this variable elimination algorithm should be applied.
   */
  val universe: Universe

  /**
   * Target elements that should not be eliminated but should be available for querying.
   */
  val targetElements: Seq[Element[_]]

  /**
   * Flag indicating whether the run time of each step should be displayed.
   */
  val showTiming: Boolean

  private def optionallyShowTiming[T](op: => T, name: String) =
    if (showTiming) timed(op, name); else op

  private def expand(): Unit =
    optionallyShowTiming(Expand(universe), "Expansion")

  private def getTargetVariables(): Seq[Variable[_]] =
    targetElements map (Variable(_))

   // The first element of FactorMap is the complete set of factors.
  // The second element maps variables to the factors mentioning that variable.
  private type FactorMap[T] = Map[Variable[_], Set[Factor[T]]]

  private def addFactor[T](factor: Factor[T], map: FactorMap[T]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, Set()) + factor))

  private def removeFactor[T](factor: Factor[T], map: FactorMap[T]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, Set()) - factor))

  private def initialFactorMap(factors: Traversable[Factor[T]]): FactorMap[T] = {
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
    factors: Set[Factor[T]],
    map: FactorMap[T]): Unit = {
    val varFactors = map(variable)
    if (debug) {
      println("*****************\nEliminating " + variable.id)
      println("Input factors:")
      for { factor <- varFactors } { println(factor.toReadableString) }
    }
    if (varFactors nonEmpty) {
      val productFactor = varFactors reduceLeft (_.product(_, semiring.product))
      val resultFactor = productFactor.sumOver(variable, semiring.sum, semiring.zero)
      varFactors foreach (removeFactor(_, map))
      addFactor(resultFactor, map)
      comparator match {
        case None => ()
        case Some(recorder) => recordingFactors ::= productFactor.recordArgMax(variable, recorder)
      }
      map -= variable
      factors --= varFactors
      if (debug) println("Result factor\n" + resultFactor.toReadableString)
      factors += resultFactor
    }
  }

  private def eliminateInOrder(
    order: List[Variable[_]],
    factors: Set[Factor[T]],
    map: FactorMap[T]): Set[Factor[T]] =
    order match {
      case Nil =>
        factors
      case first :: rest =>
        eliminate(first, factors, map)
        eliminateInOrder(rest, factors, map)
    }

  /**
   * Method for choosing the elimination order.
   * The default order chooses first the variable that
   * minimizes the number of extra factor entries that would be created when it is eliminated.
   * Override this method if you want a different rule.
   */
  def eliminationOrder(factors: Traversable[Factor[T]], toPreserve: Traversable[Variable[_]]): List[Variable[_]] = {
    val eliminableVars = (Set[Variable[_]]() /: factors)(_ ++ _.variables) -- toPreserve
    var initialGraph = new VEGraph(factors)
    val candidates = new HeapPriorityMap[Variable[_], Long]
    eliminableVars foreach (v => candidates += v -> initialGraph.score(v))
    eliminationOrderHelper(candidates, toPreserve, initialGraph, List())
  }

  @tailrec private def eliminationOrderHelper(candidates: PriorityMap[Variable[_], Long],
    toPreserve: Traversable[Variable[_]],
    graph: VEGraph,
    accum: List[Variable[_]]): List[Variable[_]] = {
    if (candidates.isEmpty) accum.reverse
    else {
      val best = candidates.extractMin()._1
      // do not read the best variable after it has been removed, and do not add the preserved variables
      val touched = graph.info(best).neighbors - best -- toPreserve
      val nextGraph = graph.eliminate(best)
      touched foreach (v => candidates += v -> graph.score(v))
      eliminationOrderHelper(candidates, toPreserve, nextGraph, best :: accum)
    }
  }

  private[figaro] def ve(): Unit = {
    expand()
    val targetVariables = getTargetVariables()
    val allFactors = optionallyShowTiming(getFactors(targetVariables), "Getting factors")
    recordingFactors = List()
    if (debug) {
      println("*****************\nStarting factors\n")
      allFactors.foreach((f: Factor[_]) => println(f.toReadableString))
    }
    val order = optionallyShowTiming(eliminationOrder(allFactors, targetVariables), "Computing elimination order")
    val factorsAfterElimination =
      optionallyShowTiming(eliminateInOrder(order, Set(allFactors: _*), initialFactorMap(allFactors)), "Elimination")
    if (debug) println("*****************")
    if (debug) factorsAfterElimination foreach (f => println(f.toReadableString))
    optionallyShowTiming(finish(factorsAfterElimination, order), "Finalizing")
    if (debug) targetFactors.values foreach (f => println(f.toReadableString))
  }

  protected[figaro] var targetFactors: Map[Element[_], Factor[T]] = Map()

  /**
   * All implementation of variable elimination must specify what to do after variables have been eliminated.
   */
  def finish(factorsAfterElimination: Set[Factor[T]], eliminationOrder: List[Variable[_]]): Unit

  def run() = ve()

}

/**
 * Variable elimination over probabilistic factors.
 */
trait ProbabilisticVariableElimination extends VariableElimination[Double] {
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

/**
 * Variable elimination algorithm that computes the conditional probability of query elements.
 * 
 */
class ProbQueryVariableElimination(universe: Universe, targets: Element[_]*)(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends ProbQueryAlgorithm(universe, targets: _*)
  with OneTimeProbQuery
  with ProbabilisticVariableElimination {
  val targetElements = targets
  val semiring = SumProductSemiring
  private def marginalizeToTarget(factor: Factor[Double], target: Element[_]): Unit = {
    val unnormalizedTargetFactor = factor.marginalizeTo(Variable(target), semiring.sum, semiring.zero)
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, _ + _)
    val targetFactor = new Factor[Double](unnormalizedTargetFactor.variables)
    unnormalizedTargetFactor.mapTo((d: Double) => d / z, targetFactor)
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[Double]) =
    targets foreach (marginalizeToTarget(resultFactor, _))

  private def makeResultFactor(factorsAfterElimination: Set[Factor[Double]]): Factor[Double] =
    factorsAfterElimination reduceLeft (_.product(_, semiring.product))

  def finish(factorsAfterElimination: Set[Factor[Double]], eliminationOrder: List[Variable[_]]) =
    marginalize(makeResultFactor(factorsAfterElimination))

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    val targetVar = Variable(target)
    val dist = targetVar.range.zipWithIndex map (pair => (factor.get(List(pair._2)), pair._1))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }
}

object VariableElimination {
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
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) { override val debug = true }
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
}
