/*
 * FairCoinTest.scala  
 * Bayesian network examples tests.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.test._

class FairCoinTest extends WordSpec with Matchers {
  "A simple FairCoinTest" should {
    "produce the correct probability under expectation maximization" taggedAs (ExampleTest) in {
     test()
    }

  }

  //This is an easy way of representing 100 coin flips.
  //An 'H' represents 'heads' and 'T' represents tails.
  //There are 62 'H's and 38 'T's.
  val data = Seq(
    'H', 'H', 'H', 'T', 'H', 'H', 'T', 'H', 'T', 'H', 'H', 'T', 'H',
    'T', 'H', 'H', 'T', 'H', 'T', 'T', 'H', 'T', 'H', 'H', 'T', 'H', 'H', 'H', 'T',
    'T', 'H', 'H', 'T', 'H', 'T', 'H', 'T', 'T', 'H', 'T', 'H', 'H', 'H', 'H', 'H',
    'H', 'H', 'H', 'H', 'T', 'H', 'T', 'H', 'H', 'T', 'H', 'H', 'H', 'H', 'H',
    'H', 'T', 'H', 'H', 'H', 'T', 'H', 'H', 'H', 'H', 'H', 'H', 'H',
    'H', 'H', 'H', 'H', 'H', 'H', 'H', 'T', 'H','T',
     'H', 'H', 'H')

  def Toss(fairness: AtomicBeta): AtomicFlip = {
    val f = fairness.getLearnedElement
    f
  }

  def test(): Unit = {
    Universe.createNew()

    val fairness = BetaParameter(1, 1)
    data.foreach {
      d =>
        val f = Flip(fairness)
        if (d == 'H') {
          f.observe(true)
        } else if (d == 'T') {
          f.observe(false)
        }

    }

    val learningAlgorithm = ExpectationMaximization(fairness)
    learningAlgorithm.start
    /*
     * This will create a flip having a probability of 'true' learned from the input data. 
     */
    val coin = fairness.getLearnedElement
    println("The probability of a coin with this fairness showing 'heads' is: ")
    println(coin.prob)
    //62/(62+38)
    coin.prob should be (0.72 +- (0.01))
    val t1 = fairness.getLearnedElement
    val t2 = fairness.getLearnedElement
    
    val equal = t1 === t2

    val alg = VariableElimination(equal)
    alg.start()
    println("The probability of two coins that exhibit this fairness showing the same side is: " + alg.probability(equal, true))
    //.62*.62 + .38*.38
    alg.probability(equal, true) should be (0.60 +-(0.01))
    
    alg.kill()

  }
}
