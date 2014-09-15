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
import com.cra.figaro.algorithm.lazyfactored.Extended
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
  
  /**
   * Makes a utility factor an element designated as a utility. This is factor of a tuple (Double, Double)
   * where the first value is 1.0 and the second is a possible utility of the element 
   */
  def makeUtilFactor(e: Element[_]): Factor[(Double, Double)] = {
    val f = Factory.make[(Double, Double)](List(Variable(e)))
    f.fillByRule((l: List[Any]) => (1.0, l.asInstanceOf[List[Extended[Double]]](0).value))
    f
  }


  /* Even though utility nodes are eliminated, we need to create factors for them and anything they use. */
  override def starterElements = getUtilityNodes ::: targetElements

  /**
   * Create the factors for decision factors. Each factor is hardcoded as a tuple of (Double, Double), 
   * where the first value is the probability and the second is the utility. 
   */ 
  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[(Double, Double)]] = {
    if (debug) {
      println("Elements (other than utilities) appearing in factors and their ranges:")
      for { element <- neededElements } { 
        println(Variable(element).id + "(" + element.name.string + "@" + element.hashCode + ")" + ": " + element + ": " + Variable(element).range.mkString(",")) 
      }
    }

    Factory.removeFactors()
    val thisUniverseFactorsExceptUtil = neededElements flatMap (Factory.make(_))
    // Make special utility factors for utility elements
    val thisUniverseFactorsUtil = getUtilityNodes map (makeUtilFactor(_))

    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield Factory.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))

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
    val factor = Factory.make[(Double, Double)](f.variables)
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
class ProbQueryVariableEliminationDecision[T, U](override val universe: Universe, utilityNodes: List[Element[_]], target: Element[_])(
    val showTiming: Boolean,
    val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends OneTimeProbQuery
  with ProbabilisticVariableEliminationDecision
  with DecisionAlgorithm[T, U] {
  lazy val queryTargets = List(target)

  /**
   *  The variable elimination eliminates all variables except on all decision nodes and their parents. 
   *  Thus the target elements is both the decision element and the parent element
   */
  val targetElements = List(target, target.args(0)) 

  def getUtilityNodes = utilityNodes

  private var finalFactors: Factor[(Double, Double)] = Factory.make[(Double, Double)](List[Variable[_]]())

  /* Marginalizes the final factor using the semiring for decisions
   * 
   */
  private def marginalizeToTarget(factor: Factor[(Double, Double)], target: Element[_]): Unit = {
    val unnormalizedTargetFactor = factor.marginalizeTo(semiring, Variable(target))
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, (x: (Double, Double), y: (Double, Double)) => (x._1 + y._1, 0.0))
   //val targetFactor = Factory.make[(Double, Double)](unnormalizedTargetFactor.variables)
    val targetFactor = unnormalizedTargetFactor.mapTo((d: (Double, Double)) => (d._1 / z._1, d._2), unnormalizedTargetFactor.variables)
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[(Double, Double)]) =
    queryTargets foreach (marginalizeToTarget(resultFactor, _))

  private def makeResultFactor(factorsAfterElimination: Set[Factor[(Double, Double)]]): Factor[(Double, Double)] = {
    // It is possible that there are no factors (this will happen if there are no decisions or utilities).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    factorsAfterElimination.foldLeft(Factor.unit(semiring))(_.product(_, semiring))
  }

  def finish(factorsAfterElimination: Set[Factor[(Double, Double)]], eliminationOrder: List[Variable[_]]) =
    finalFactors = makeResultFactor(factorsAfterElimination)

  /**
   * Returns distribution of the target, ignoring utilities
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    val targetVar = Variable(target)
    val dist = targetVar.range.filter(_.isRegular).map(_.value).zipWithIndex map (pair => (factor.get(List(pair._2))._1, pair._1))
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
      val parent = parentVariable.range(indices(indexOParent(0))).value.asInstanceOf[T]
      val decision = decisionVariable.range(indices(indexOfDecision(0))).value.asInstanceOf[U]
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
