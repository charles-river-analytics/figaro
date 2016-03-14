/*
 * MarkovChain.scala 
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

object MarkovChain {
  val length = 90
  val ourPossession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  ourPossession(0) = Flip(0.5)
  for { minute <- 1 until length } {
    ourPossession(minute) = If(ourPossession(minute - 1), Flip(0.6), Flip(0.3))
  }

  def main(args: Array[String]) {
    println("Probability we have possession at time step 5")
    println("Prior probability: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(4).observe(true)
    println("After observing that we have possession at time step 4: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(3).observe(true)
    println("After observing that we have possession at time step 3: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(6).observe(true)
    println("After observing that we have possession at time step 6: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(7).observe(true)
    println("After observing that we have possession at time step 7: " + VariableElimination.probability(ourPossession(5), true))
  }
}

class MarkovChainTest extends WordSpec with Matchers {
  Universe.createNew()
  val prior = VariableElimination.probability(MarkovChain.ourPossession(5), true)
  MarkovChain.ourPossession(4).observe(true)
  val posStep2 = VariableElimination.probability(MarkovChain.ourPossession(5), true)
  MarkovChain.ourPossession(3).observe(true)
  val posStep1 = VariableElimination.probability(MarkovChain.ourPossession(5), true)
  MarkovChain.ourPossession(6).observe(true)
  val posStep0 = VariableElimination.probability(MarkovChain.ourPossession(5), true)
  MarkovChain.ourPossession(7).observe(true)
  val posStep3 = VariableElimination.probability(MarkovChain.ourPossession(5), true)
    
  "Markov Chain" should {
    "produce a probability we have posession at time step 5: 0.42874500000000004" taggedAs (BookExample) in {
      prior should be(0.42874500000000004)
    }
    "after observing that we have possession at time step 4: 0.6" taggedAs (BookExample) in {
      posStep2 should be(0.6)
    }
    "after observing that we have possession at time step 3: 0.600 +- 0.001" taggedAs (BookExample) in {
      posStep1 should be(0.600 +- 0.001)
    }
    "after observing that we have possession at time step 6: 0.75" taggedAs (BookExample) in {
      posStep0 should be(0.75)
    }
    "after observing that we have possession at time step 7: 0.75" taggedAs (BookExample) in {
      posStep3 should be(0.75)
    }
  }
}
