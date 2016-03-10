/*
 * DynamicBayesianNetwork.scala 
 * Book example unit test.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 26, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.book.chap08

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.compound.{If, CPD}
import com.cra.figaro.algorithm.factored.VariableElimination
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object DynamicBayesianNetwork {
  val length = 91
  val winning: Array[Element[String]] = Array.fill(length)(Constant(""))
  val confident: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  val ourPossession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  val goal: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  val scoreDifferential: Array[Element[Int]] = Array.fill(length)(Constant(0))
  confident(0) = Flip(0.4)
  scoreDifferential(0) = Constant(0)
  for { minute <- 1 until length } {
    winning(minute) =
      Apply(scoreDifferential(minute - 1), (i: Int) => if (i > 0) "us" else if (i < 0) "them" else "none")
    confident(minute) =
      CPD(confident(minute - 1), winning(minute),
          (true, "us") -> Flip(0.9),
          (true, "none") -> Flip(0.7),
          (true, "them") -> Flip(0.5),
          (false, "us") -> Flip(0.5),
          (false, "none") -> Flip(0.3),
          (false, "them") -> Flip(0.1))
    ourPossession(minute) = If(confident(minute), Flip(0.7), Flip(0.3))
    goal(minute) =
      CPD(ourPossession(minute), confident(minute),
          (true, true) -> Flip(0.04),
          (true, false) -> Flip(0.01),
          (false, true) -> Flip(0.045),
          (false, false) -> Flip(0.02))
    scoreDifferential(minute) =
      If(goal(minute),
         Apply(ourPossession(minute), scoreDifferential(minute - 1),
               (poss: Boolean, diff: Int) => if (poss) (diff + 1).min(5) else (diff - 1).max(-5)),
         scoreDifferential(minute - 1))
  }

  def main(args: Array[String]) {
    println("Probability we will win the game")
    println("Prior probability: " +
      VariableElimination.probability(scoreDifferential(length - 1), (i: Int) => i > 0))
    ourPossession(4).observe(true)
    goal(4).observe(true)
    println("After observing that we scored at time step 4: " +
      VariableElimination.probability(scoreDifferential(length - 1), (i: Int) => i > 0))
  }
}

class DynamicBayesianNetworkTest extends WordSpec with Matchers {
  Universe.createNew()
  val prior = VariableElimination.probability(DynamicBayesianNetwork.scoreDifferential(DynamicBayesianNetwork.length - 1), (i: Int) => i > 0)
  DynamicBayesianNetwork.ourPossession(4).observe(true)
  DynamicBayesianNetwork.goal(4).observe(true)
  val afterScore = VariableElimination.probability(DynamicBayesianNetwork.scoreDifferential(DynamicBayesianNetwork.length - 1), (i: Int) => i > 0)

  "Dynamic Bayesian Network" should {
    "produce a prior probability we will win the game = 0.400617362192 +- 0.000000000002" taggedAs (BookExample) in {
      prior should be(0.400617362192 +- 0.000000000002)
    }
    "produce a probability after observing that we scored at time step 4 = 0.731889855275 +- 0.000000000002" taggedAs (BookExample) in {
      afterScore should be(0.731889855275 +- 0.000000000002)
    }
  }
}
