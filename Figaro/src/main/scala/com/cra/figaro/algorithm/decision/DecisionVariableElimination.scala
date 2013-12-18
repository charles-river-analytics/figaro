/*
 * DecisionVariableElimination.scala
 * Variable elimination for Decisions algorithm.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection.mutable.{ Map, Set }
import scala.language.existentials

/* Trait only extends for double utilities. User needs to provide another trait or convert utilities to double
 * in order to use
 * 
 */
/**
 * Trait for Decision based Variable Elimination. This implementation is hardcoded to use
 * Double utilities
 */
trait ProbabilisticVariableEliminationDecision extends VariableElimination[(Double, Double)] {

  /** Retrieve utility nodes in the model
   */
  /* Implementations must define this */
  def getUtilityNodes: List[Element[_]]
  
  /**
   * Semiring for Decisions uses a sum-product-utility semiring
   */
  override val semiring = SumProductUtilitySemiring
  
  /* Have to expand utility elements that are chains so we get all elements that could be used as utility 
   * Note this is not tail recursive. Stack overflow is not that much of a concern since large models
   * are not efficient using variable elimination anyways
   */
  private def expandUtility(util: List[Element[_]]): Set[Element[_]] = {
    def expand(e: Element[_], curr: Set[Element[_]]): Set[Element[_]] = {
      e match {
        case c: Chain[_, _] => (curr) ++ Expand(c.universe).getMap(c).flatMap(s => expand(s._2, Set()))
        case _ => curr + e
      }
    }
    (Set() ++ util).flatMap(e => expand(e, Set()))
  }

  /**
   * Makes a utility factor an element designated as a utility. This is factor of a tuple (Double, Double)
   * where the first value is 1.0 and the second is a possible utility of the element 
   */
  def makeUtilFactor(e: Element[_]): Factor[(Double, Double)] = {
    val f = new Factor[(Double, Double)](List(Variable(e)))
    f.fillByRule((l: List[Any]) => (1.0, l.asInstanceOf[List[Double]](0)))
    f
  }

  /**
   * Create the factors for decision factors. Each factor is hardcoded as a tuple of (Double, Double), 
   * where the first value is the probability and the second is the utility. 
   */ 
  def getFactors(targetVariables: Seq[Variable[_]]): List[Factor[(Double, Double)]] = {
    val allElements = universe.activeElements
    // have to expand utility node chains since we need to find the elements who's value is actually used as utility
    val allUtilityNodes = expandUtility(getUtilityNodes).toList

    // Generate normal factors separately for utility variables and everything else
    val thisUniverseFactorsExceptUtil = allElements flatMap (ProbFactor.make(_))
    // Make special utility factors for utility elements
    val thisUniverseFactorsUtil = allUtilityNodes map (makeUtilFactor(_))

    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield ProbFactor.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))

    // Convert all non-utility factors from standard factors to decision factors, ie, factors are now tuples of (Double, _)
    val thisUniverseFactorsExceptUtil_conv = thisUniverseFactorsExceptUtil.map(s => convert(s, false))
    val thisUniverseFactorsUtil_conv = thisUniverseFactorsUtil
    val dependentUniverseFactors_conv = dependentUniverseFactors.map(s => convert(s, false))

    dependentUniverseFactors_conv ::: thisUniverseFactorsExceptUtil_conv ::: thisUniverseFactorsUtil_conv
  }

  /*
   * Converts a factor created by ProbFactor into a tuple of (Prob, E[Utility]), where E[Utility] is zero for
   * all non-utility nodes, and Prob is 1 for all utility nodes
   */
  private def convert(f: Factor[Double], utility: Boolean): Factor[(Double, Double)] = {
    val factor = new Factor[(Double, Double)](f.variables)
    val allIndices = f.allIndices

    allIndices.foreach { k: List[Int] =>
      val p = f.get(k)
      val v = if (utility) {
        if (f.variables.length > 1) throw new IllegalUtilityNodeException
        f.variables(0).range(k(0)).asInstanceOf[Double]
      } else {
        0.0
      }
      factor.set(k, (p, v))
    }
    factor
  }

}

/**
 * Decision VariableElimination algorithm that computes the expected utility of decision elements using the default
 * elimination order.
 */
class ProbQueryVariableEliminationDecision[T, U](universe: Universe, utilityNodes: List[Element[_]], target: Element[_])(
    val showTiming: Boolean,
    val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends ProbQueryAlgorithm(universe, List(target): _*)
  with OneTimeProbQuery
  with ProbabilisticVariableEliminationDecision
  with DecisionAlgorithm[T, U] {

  /**
   *  The variable elimination eliminates all variables except on all decision nodes and their parents. 
   *  Thus the target elements is both the decision element and the parent element
   */
  val targetElements = List(target, target.args(0)) 

  def getUtilityNodes = utilityNodes

  private var finalFactors: Factor[(Double, Double)] = new Factor[(Double, Double)](List[Variable[_]]())

  /* Marginalizes the final factor using the semiring for decisions
   * 
   */
  private def marginalizeToTarget(factor: Factor[(Double, Double)], target: Element[_]): Unit = {
    val unnormalizedTargetFactor = factor.marginalizeTo(Variable(target), semiring.sum, semiring.zero)
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, (x: (Double, Double), y: (Double, Double)) => (x._1 + y._1, 0.0))
    val targetFactor = new Factor[(Double, Double)](unnormalizedTargetFactor.variables)
    unnormalizedTargetFactor.mapTo((d: (Double, Double)) => (d._1 / z._1, d._2), targetFactor)
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[(Double, Double)]) =
    targets foreach (marginalizeToTarget(resultFactor, _))

  private def makeResultFactor(factorsAfterElimination: Set[Factor[(Double, Double)]]): Factor[(Double, Double)] =
    factorsAfterElimination reduceLeft (_.product(_, semiring.product))

  def finish(factorsAfterElimination: Set[Factor[(Double, Double)]], eliminationOrder: List[Variable[_]]) =
    finalFactors = makeResultFactor(factorsAfterElimination)

  /**
   * Returns distribution of the target, ignoring utilities
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    val targetVar = Variable(target)
    val dist = targetVar.range.zipWithIndex map (pair => (factor.get(List(pair._2))._1, pair._1))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  /**
   * Returns expectation of the target, ignoring utilities
   */
  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }

  /** 
   *  Returns the computed utility of all parent/decision tuple values. For VE, these are not samples
   *  but the actual computed expected utility for all combinations of the parent and decision
   */
  def computeUtility(): scala.collection.immutable.Map[(T, U), DecisionSample] = computeStrategy(finalFactors)

  /*
   * Converts the final factor into a map of parent/decision values and expected utility
   */
  private def computeStrategy(factor: Factor[(Double, Double)]) = {
    val strat = Map[(T, U), DecisionSample]()

    //find the variable associated with the decision
    val decisionVariable = factor.variables.filter(_.asInstanceOf[ElementVariable[_]].element == target)(0)

    // find the variables of the parents.
    val parentVariable = factor.variables.filterNot(_ == decisionVariable)(0)

    // index of the decision variable     

    val indexOfDecision = indices(factor.variables, decisionVariable)
    val indexOParent = indices(factor.variables, parentVariable)

    for { indices <- factor.allIndices } {

      /* for each index in the list of indices, strip out the decision variable index, 
       * and retrieve the map entry for the parents. If the factor value is greater than
       * what is currently stored in the strategy map, replace the decision with the new one from the factor
       */
      val parent = parentVariable.range(indices(indexOParent(0))).asInstanceOf[T]
      val decision = decisionVariable.range(indices(indexOfDecision(0))).asInstanceOf[U]
      val utility = factor.get(indices)._2

      strat += (parent, decision) -> DecisionSample(utility, 1.0)

    }
    strat.toMap

  }

}

object DecisionVariableElimination {

    /* Checks conditions of Decision Usage
   * 1. Double utilities
   */
  private[decision] def usageCheck(utilityNodes: List[Element[_]], target: Decision[_, _]): Unit = {
    utilityNodes.foreach { u =>
      u.value match {
        case d: Double => 1
        case _ => throw new IllegalArgumentException("Only double utilities are allowed")
      }
    }
  }

  /**
   * Create a decision variable elimination instance with the given decision variables and indicated utility
   * nodes 
   */
  def apply[T, U](utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate()) // need initial values for the utility nodes before the usage check
    usageCheck(utilityNodes, target)
    new ProbQueryVariableEliminationDecision[T, U](universe, utilityNodes, target)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
  }
  /**
   * Create a decision variable elimination algorithm with  the given decision variables and indicated utility
   * nodes and using the given dependent universes in the current default universe.
   */
  def apply[T, U](dependentUniverses: List[(Universe, List[NamedEvidence[_]])], utilityNodes: List[Element[_]], target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate()) // need initial values for the utility nodes before the usage check
    usageCheck(utilityNodes, target)
    new ProbQueryVariableEliminationDecision[T, U](universe, utilityNodes, target)(
      false,
      dependentUniverses,
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
  }
  /**
   * Create a decision variable elimination algorithm with  the given decision variables and indicated utility
   * nodes and using the given dependent universes in the current default universe. Use the given dependent 
   * algorithm function to determine the algorithm to use to compute probability of evidence in each dependent universe.
   */
  def apply[T, U](
    dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    utilityNodes: List[Element[_]],
    target: Decision[T, U])(implicit universe: Universe) = {
    utilityNodes.foreach(_.generate()) // need initial values for the utility nodes before the usage check
    usageCheck(utilityNodes, target)
    new ProbQueryVariableEliminationDecision[T, U](universe, utilityNodes, target)(
      false,
      dependentUniverses,
      dependentAlgorithm)
  }
}
