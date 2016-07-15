/*
 * FairDiceTest.scala  
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

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.learning.EMWithBP
import com.cra.figaro.language.Apply
import com.cra.figaro.language.Name.stringToName
import com.cra.figaro.language.Select
import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.patterns.learning.ModelParameters
import com.cra.figaro.patterns.learning.ParameterCollection
import com.cra.figaro.test.tags.Example

class FairDiceTest extends WordSpec with Matchers {
  "A simple FairDiceTest" should {
    "produce the correct probability under expectation maximization" taggedAs (Example) in {
      test()
    }

  }

  class DiceModel(val parameters: ParameterCollection, val data: Seq[(Int, Int, Int)], val outcomes: List[Int]) {

  val sum = (i1: Int, i2: Int) => i1 + i2
  val trials = for (datum <- data) yield {
    val die1 = Select(parameters.get("fairness" + datum._1), outcomes: _*)
    val die2 = Select(parameters.get("fairness" + datum._2), outcomes: _*)
    Apply(die1, die2, sum)
  }
}
  
  val universe = Universe.createNew()

  /*
  *The possible outcomes of a dice roll.
  */
  val outcomes = List(1, 2, 3, 4, 5, 6)

  /* This data represents 50 rolls of two dice.
   * The first two numbers in each triple are a choice of dice 1, 2 or 3.
   * The second number is the sum of the two dice.
   * D1 always lands on 1
   * D2 always lands on 2
   * D3 always lands on 6
   */
  val data = Seq((2, 3, 8), (1, 3, 7), (1, 2, 3), (1, 2, 3), (1, 2, 3), (1, 2, 3),
    (1, 3, 7), (1, 3, 7), (2, 3, 8), (2, 3, 8), (2, 3, 8), (2, 3, 8),
    (2, 3, 8), (1, 2, 3), (2, 3, 8), (1, 2, 3), (1, 2, 3), (2, 3, 8), (1, 3, 7),
    (2, 3, 8), (1, 3, 7), (1, 3, 7), (1, 2, 3), (2, 3, 8), (1, 3, 7), (2, 3, 8),
    (2, 3, 8), (1, 2, 3), (2, 3, 8), (1, 2, 3), (1, 3, 7), (1, 2, 3), (1, 3, 7),
    (1, 2, 3), (2, 3, 8), (2, 3, 8), (1, 3, 7), (1, 2, 3), (1, 2, 3), (1, 3, 7),
    (1, 3, 7), (2, 3, 8), (2, 3, 8), (1, 2, 7), (1, 2, 7), (1, 2, 7), (2, 3, 8),
    (1, 3, 7), (1, 3, 7), (1, 2, 3))


  def test() {
    val params = ModelParameters()
    val outcomes = List(1, 2, 3, 4, 5, 6)
    /*
     * Each coin is initially assumed to be fair
     */
    val fairness1 = Dirichlet(2.0, 2.0, 2.0, 2.0, 2.0, 2.0)("fairness1", params)
    val fairness2 = Dirichlet(2.0, 2.0, 2.0, 2.0, 2.0, 2.0)("fairness2", params)
    val fairness3 = Dirichlet(2.0, 2.0, 2.0, 2.0, 2.0, 2.0)("fairness3", params)

    val model = new DiceModel(params.priorParameters, data, outcomes)

    for ((datum, trial) <- data zip model.trials) {
      trial.observe(datum._3)
    }

    val numberOfEMIterations = 10
    val numberOfBPIterations = 10
    val algorithm = EMWithBP(numberOfEMIterations, numberOfBPIterations, params)
    algorithm.start
    algorithm.stop

    println("The probabilities of seeing each side of d_1 are: ")

    val d1 = Select(params.posteriorParameters.get("fairness1"), outcomes: _*)
    val d2 = Select(params.posteriorParameters.get("fairness2"), outcomes: _*)
    val d3 = Select(params.posteriorParameters.get("fairness3"), outcomes: _*)

    val ve = VariableElimination(d1,d2,d3)
    ve.start()
    ve.stop()
    
    println("\nThe probabilities of seeing each side of d_1 are: ")
    outcomes.foreach { o => println("\t" + ve.probability(d1)(_ == o) + " -> " + o) }
    println("\nThe probabilities of seeing each side of d_2 are: ")
    outcomes.foreach { o => println("\t" + ve.probability(d2)(_ == o) + " -> " + o) }
    println("\nThe probabilities of seeing each side of d_3 are: ")
    outcomes.foreach { o => println("\t" + ve.probability(d3)(_ == o) + " -> " + o) }

    (ve.probability(d1)(_ == outcomes(0)) > .8) should be(true)
    (ve.probability(d2)(_ == outcomes(1)) > .8) should be(true)
    (ve.probability(d3)(_ == outcomes(5)) > .8) should be(true)

    ve.kill()
    algorithm.kill
  }
}
