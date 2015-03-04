/*
 * MultiDecisionAlgorithm.scala
 * Base class for Multi-decision algorithms
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

import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.decision._
import com.cra.figaro.library.decision.DecisionUtil._
import com.cra.figaro.algorithm.lazyfactored._
import scala.collection.mutable.Map
import scala.language.existentials


/**
 * Abstract class common to all multi-decision algorithms. Multi-decision algorithms implement backward induction
 * by 1) determining the order in which decisions can be computed 2) Implementing a single decision algorithm on
 * each decision (in the proper order). 
 *
 * Note: Only OneTime algorithms are supported in multi-decision algorithms.
 */
abstract class MultiDecisionAlgorithm(universe: Universe, utilityNodes: List[Element[_]], targets: List[Element[_]])
  extends OneTime {

  /**
   * List of the single decision algorithms implemented in the multi-decision algorithm.
   */
  var algList: Map[Decision[_, _], OneTimeProbQueryDecision[_, _]] = Map()

  /*
   * Get the utility map for a specific decision in the multi-decision algorithm
   */
  private def getUtility[T, U](D: Decision[T, U]) = {
    algList.get(D) match {
      case Some(alg) => alg.getUtility()
      case _ => throw new AlgorithmDecisionException
    }
  }
  /**
   * Get the utility for a specific parent and decision in the multi-decision algorithm.
   */
  def getUtility[T, U](D: Decision[T, U], p: T, d: U) = {
    algList.get(D) match {
      case Some(alg) => alg.asInstanceOf[OneTimeProbQueryDecision[T, U]].getUtility(p, d)
      case _ => throw new AlgorithmDecisionException
    }
  }

  /**
   * Computes the order in which decisions should be computed. Decision order goes from independent->dependent.
   * 
   */
  val decisionOrder = getDecisionOrder(targets, universe)

  /*
   * Function that defines how to create an instance of the inference algorithm to be used for this
   * multi-decision. Overridden by each multi-decision algorithm class.
   */
  protected def createAlg[T, U](decisionTarget: Decision[T, U],
    utilities: List[Element[_]], mv: Universe): OneTimeProbQueryDecision[T, U]

  /*
   * Runs a decision algorithm for a set of INDEPENDENT decisions. Returns a mapping of each decision
   * element to the new element that represents the expected utility of each decision 
   */
  protected def makeAlg(decisions: List[Decision[_, _]], succExpUtil: Map[Element[_], Element[_]]): Map[Element[_], Element[_]] = {
    // Loop through all the independent decisions
    decisions.map { d_old =>
      val d: Decision[d_old.PValue, d_old.DValue] = d_old.asInstanceOf[Decision[d_old.PValue, d_old.DValue]]
      universe.clearTemporaries()

      // Get all the utilites used by the element
      val utilD = utilitiesInElems(universe.usedBy(d).toList, utilityNodes)
      
      // Create a tuple element of the parent and decision
      val dummy = createDecisionDummy(d)

      // Run the algorithm, set the new strategy
      val alg = createAlg(d, utilD, universe)
      algList += (d -> alg)
      alg.universe.clearTemporaries()
      alg.start()
      // Automatically set the policy for computed decisions
      alg.setPolicy(d)

    }
    succExpUtil
  }

  /* Recursively run each decision algorithm in independent order. Setting oneStep to true will only run
   * the last decision */
  private def runMulti(decisions: List[List[Element[_]]], oneStep: Boolean): Map[Element[_], Element[_]] = {
    if (decisions.size == 1) {
      makeAlg(decisions(0).asInstanceOf[List[Decision[_, _]]], Map[Element[_], Element[_]]())
    } else {
      val curr = decisions.head.asInstanceOf[List[Decision[_, _]]]
      val rest = decisions.tail
      val newElems = runMulti(rest, oneStep)
      if (!oneStep) makeAlg(curr, newElems) else Map[Element[_], Element[_]]()
    }
  }

  def run(): Unit = run(false)

  override def cleanUp() = algList.foreach(_._2.kill)

  /**
   * Run in a debug mode where only a single decision is run each time.
   *
   */
  def run(oneStep: Boolean): Unit = runMulti(decisionOrder, oneStep)

}

