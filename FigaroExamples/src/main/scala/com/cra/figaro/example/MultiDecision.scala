/*
 * MultiDecision.scala
 * A Multi Decision Example, based on the Entrepreneur example from Koller and Friedman, 2009.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.decision._
import scala.collection.immutable.Map

/**
 * A Multi Decision Example, based on the Entrepreneur example from Koller and Friedman, 2009.
 */
object MultiDecision {
  Universe.createNew()

  val market = Select(0.5 -> 0, 0.3 -> 1, 0.2 -> 2)

  // First decision is whether or not to conduct the survey
  val test = Decision(List(true, false))

  // Create the utility element as an Apply element with the utility function
  val cost = Apply(test, (b: Boolean) => if (b) -1.0 else 0.0)

  val survey = RichCPD(test, market,
    (OneOf(false), *) -> Constant(-1),
    (OneOf(true), OneOf(0)) -> Select(0.6 -> 0, 0.3 -> 1, 0.1 -> 2),
    (OneOf(true), OneOf(1)) -> Select(0.3 -> 0, 0.4 -> 1, 0.3 -> 2),
    (OneOf(true), OneOf(2)) -> Select(0.1 -> 0, 0.4 -> 1, 0.5 -> 2))

  // second decision is whether or not to found the company
  val found = Decision(survey, List(true, false))

  // Create the utility element as an Apply element with the utility function
  def valueFcn(f: Boolean, m: Int): Double = {
    if (f) {
      m match {
        case 0 => -7.0
        case 1 => 5.0
        case 2 => 20.0
      }
    } else {
      0.0
    }
  }
  val value = Apply(found, market, valueFcn)

  def main(args: Array[String]) {

    val ve_before = VariableElimination(value, cost)
    ve_before.start()
 
    val propmaker = (mv: Universe, e: Element[_]) => ProposalScheme.default(mv)
    val alg = MultiDecisionMetropolisHastings(200000, propmaker, 20000, List(value, cost), test, found)

    alg.start()

    val ve_after = VariableElimination(value, cost)
    ve_after.start()

    println("Expected Utility Before Optimization: "
      + (ve_before.computeExpectation(value, (t: Double) => t) + ve_before.computeExpectation(cost, (t: Double) => t)))

    println("Expected Utility After Optimization: "
      + (ve_after.computeExpectation(value, (t: Double) => t) + ve_after.computeExpectation(cost, (t: Double) => t)))

    println("Optimal Decisions for test: ")
    println("Constant -> " + test.getPolicy(0) + ", (" + alg.getUtility(test, 0, false).norm + " vs " + alg.getUtility(test, 0, true).norm + ")")
    println()

    println("Optimal Decisions for found: ")
    println("-1 -> " + found.getPolicy(-1) + ", (" + alg.getUtility(found, -1, false).norm + " vs " + alg.getUtility(found, -1, true).norm + ")")
    println("0 -> " + found.getPolicy(0) + ", (" + alg.getUtility(found, 0, false).norm + " vs " + alg.getUtility(found, 0, true).norm + ")")
    println("1 -> " + found.getPolicy(1) + ", (" + alg.getUtility(found, 1, false).norm + " vs " + alg.getUtility(found, 1, true).norm + ")")
    println("2 -> " + found.getPolicy(2) + ", (" + alg.getUtility(found, 2, false).norm + " vs " + alg.getUtility(found, 2, true).norm + ")")

    alg.kill
  }
} 






