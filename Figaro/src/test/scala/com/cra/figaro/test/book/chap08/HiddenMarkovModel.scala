/*
 * HiddenMarkovModel.scala 
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
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object HiddenMarkovModel {
  val length = 90
  val confident: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  val ourPossession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  confident(0) = Flip(0.4)
  for { minute <- 1 until length } {
    confident(minute) = If(confident(minute - 1), Flip(0.6), Flip(0.3))
  }
  for { minute <- 0 until length } {
    ourPossession(minute) = If(confident(minute), Flip(0.7), Flip(0.3))
  }

  def main(args: Array[String]) {
    println("Probability we are confident at time step 2")
    println("Prior probability: " + VariableElimination.probability(confident(2), true))
    ourPossession(2).observe(true)
    println("After observing current possession at time step 2: " + VariableElimination.probability(confident(2), true))
    ourPossession(1).observe(true)
    println("After observing previous possession at time step 1: " + VariableElimination.probability(confident(2), true))
    ourPossession(0).observe(true)
    println("After observing previous possession at time step 0: " + VariableElimination.probability(confident(2), true))
    ourPossession(3).observe(true)
    println("After observing future possession at time step 3: " + VariableElimination.probability(confident(2), true))
    ourPossession(4).observe(true)
    println("After observing future possession at time step 4: " + VariableElimination.probability(confident(2), true))
  }
}

class HiddenMarkovModelTest extends WordSpec with Matchers {
  Universe.createNew()
  val prior = VariableElimination.probability(HiddenMarkovModel.confident(2), true)
  HiddenMarkovModel.ourPossession(2).observe(true)
  val posStep2 = VariableElimination.probability(HiddenMarkovModel.confident(2), true)
  HiddenMarkovModel.ourPossession(1).observe(true)
  val posStep1 = VariableElimination.probability(HiddenMarkovModel.confident(2), true)
  HiddenMarkovModel.ourPossession(0).observe(true)
  val posStep0 = VariableElimination.probability(HiddenMarkovModel.confident(2), true)
  HiddenMarkovModel.ourPossession(3).observe(true)
  val posStep3 = VariableElimination.probability(HiddenMarkovModel.confident(2), true)
  HiddenMarkovModel.ourPossession(4).observe(true)
  val posStep4 = VariableElimination.probability(HiddenMarkovModel.confident(2), true)
    
  "Hidden Markov Model" should {
    "produce a prior probability we are confident at time step 2: 0.426 +- .001" taggedAs (BookExample) in {
      prior should be(0.426 +- .001)
    }
    "after observing current possession at time step 2: 0.6339285714285714" taggedAs (BookExample) in {
      posStep2 should be(0.6339285714285714)
    }
    "after observing previous possession at time step 1: 0.6902173913043478" taggedAs (BookExample) in {
      posStep1 should be(0.6902173913043478)
    }
    "after observing previous possession at time step 0: 0.7046460176991151" taggedAs (BookExample) in {
      posStep0 should be(0.7046460176991151)
    }
    "after observing future possession at time step 3: 0.7541436464088398" taggedAs (BookExample) in {
      posStep3 should be(0.7541436464088398)
    }
    "after observing future possession at time step 4: 0.7663786503335885" taggedAs (BookExample) in {
      posStep4 should be(0.7663786503335885)
    }
  }
}
