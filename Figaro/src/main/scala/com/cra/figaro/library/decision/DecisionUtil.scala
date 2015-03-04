/*
 * DecisionUtil.scala
 * Utility functions that are used in decision inference.
 * 
 * Created By:      Brian Ruttenberg(bruttenberg@cra.com)
 * Creation Date:   Oct 4, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.decision

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.^^

class AlgorithmDecisionException extends AlgorithmException
class DecisionIllegalArgumentException(S: String) extends IllegalArgumentException(S)

class ParentValueNotFoundException extends AlgorithmDecisionException
class IllegalUtilityNodeException extends AlgorithmDecisionException
class IllegalDecisionNetwork(S: String) extends DecisionIllegalArgumentException(S: String)

/**
 * Utility functions that are used in decision inference.
 *
 */
object DecisionUtil {

  /**
   * Computes the partial order of dependent decisions.
   */
  def getDecisionOrder(Decisions: Traversable[Element[_]], universe: Universe): List[List[Element[_]]] =
    universe.layers(Decisions)

  /**
   * Returns the utilities in a list of elements from a reference list of utilities.
   */
  def utilitiesInElems(Elems: List[Element[_]], U: List[Element[_]]): List[Element[_]] =
    Elems.intersect(U)

  /**
   * Computes the set of elements that the given list of utilities depends upon for computation.
   */
  def utilitiesUse(U: Traversable[Element[_]], universe: Universe): Set[Element[_]] =
    U.foldLeft(Set[Element[_]]())((s, u) => s union universe.uses(u)).toSet

  /**
   * Computes all the relevant decisions that come before a decision in the decision order.
   * It does not return decisions that are partially ordered with D.
   *  */
  def getPredDecisions(D: Element[_], Order: List[List[Element[_]]], universe: Universe) = {
    val contain = Order.map(s => s.contains(D)).indexOf(true)
    Order.splitAt(contain)._1.flatten.diff(List(D)) intersect universe.uses(D).toList
  }

  /**
   * Computes the decisions that come DIRECTLY after a decision.
   */
  def getSuccDecisions(D: Element[_], Order: List[List[Element[_]]], universe: Universe) = {
    val contain = Order.map(s => s.contains(D)).indexOf(true)
    val allD = Order.splitAt(contain)._2.flatten.diff(List(D)) intersect universe.usedBy(D).toList
    (allD /: allD)((c, n) => c.diff(universe.usedBy(n).toList))
  }

  /**
   * Computes the relevant utility elements for a specific decision.
   */
  def getReleventUtil(D: Element[_], utilities: List[Element[_]], Order: List[List[Element[_]]], universe: Universe) = {
    val utilD = utilitiesInElems(universe.usedBy(D).toList, utilities)
    val succUtil = getSuccDecisions(D, Order, universe).foldLeft(Set[Element[_]]()) { (s, u) =>
      s union utilitiesInElems(universe.usedBy(u).toList, utilities).toSet
    }
    utilD.diff(succUtil.toList)
  }

  /**
   * Computes all the elements that need to be simulated in order to compute a strategy for a decision.
   */
  def getElemsForDecision(D: Element[_], utilities: List[Element[_]], Order: List[List[Element[_]]], universe: Universe) = {
    val utilUses = getReleventUtil(D, utilities, Order, universe).foldLeft(Set[Element[_]]()) { (s, u) =>
      s union universe.uses(u).toSet union Set(u)
    }
    val predDecUses = getPredDecisions(D, Order, universe).foldLeft(Set[Element[_]]()) { (s, u) =>
      s union (u match {
        case n: NonCachingDecision[_, _] => universe.uses(u).toSet
        case c: CachingDecision[_, _]    => universe.uses(u).toSet
      })
    }
    val succ = universe.usedBy(D)
    val succDecUsedBy = getSuccDecisions(D, Order, universe).foldLeft(Set[Element[_]]()) { (s, u) =>
      s union universe.usedBy(u).toSet
    }
    val succDecUses = getSuccDecisions(D, Order, universe).foldLeft(Set[Element[_]]()) { (s, u) =>
      s union universe.uses(u).toSet
    }
    val a = getSuccDecisions(D, Order, universe)

    (utilUses union succDecUses union getSuccDecisions(D, Order, universe).toSet).diff(predDecUses).toList

  }

  /**
   * Creates a dummy element pair of (parent, decision) used for inference algorithms.
   */
  def createDecisionDummy[T, U](target: Decision[T, U]) = ^^(target.args(0).asInstanceOf[Element[T]], target)

}
