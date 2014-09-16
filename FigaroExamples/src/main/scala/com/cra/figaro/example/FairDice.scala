/*
 * FairDice.scala
 * A model for learning fairness of three dice.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._

/**
 * A model for learning fairness of three dice. In each trial, choose two die randomly and their sum is observed.
 * This model will learn the fairness of each die.
 */
object DiceExample {

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


  def trial(p1: AtomicDirichlet, p2: AtomicDirichlet, result: Int) =
    {
      val sum = (i1: Int, i2: Int) => i1 + i2
      val die1 = Select(p1, outcomes: _*)
      val die2 = Select(p2, outcomes: _*)
      val t = Apply(die1, die2, sum)
      t.observe(result)
    }

  def main(args: Array[String]) {

    
    /*
     * Each coin is initially assumed to be fair
     */
    val fairness1 = DirichletParameter(2.0,2.0,2.0,2.0,2.0,2.0)
    val fairness2 = DirichletParameter(2.0,2.0,2.0,2.0,2.0,2.0)
    val fairness3 = DirichletParameter(2.0,2.0,2.0,2.0,2.0,2.0)
    val parameters = Seq(fairness1, fairness2, fairness3)
 
    data.foreach {
      d =>
        if (d._1 == 1 && d._2 == 2) {
          trial(parameters(0), parameters(1), d._3)
        } else if (d._1 == 1 && d._2 == 3) {
          trial(parameters(0), parameters(2), d._3)
        } else {
          trial(parameters(1), parameters(2), d._3)
        }

    }

    val numberOfEMIterations = 10
	val numberOfBPIterations = 10
	val algorithm = EMWithBP(numberOfEMIterations, numberOfBPIterations, parameters: _*)
    algorithm.start
	algorithm.stop
    val die1 = Select(fairness1.MAPValue.view(0,fairness1.MAPValue.size).toList, outcomes) 
    val die2 = Select(fairness2.MAPValue.view(0,fairness2.MAPValue.size).toList, outcomes)
    val die3 = Select(fairness3.MAPValue.view(0,fairness3.MAPValue.size).toList, outcomes)

    println("The probabilities of seeing each side of d_1 are: ")
    val probsAndSides1 = die1.probs zip die1.outcomes
    probsAndSides1.map(a => println("\t" + a._1 + " -> " + a._2))
    println("\nThe probabilities of seeing each side of d_2 are: ")
    val probsAndSides2 = die2.probs zip die2.outcomes
    probsAndSides2.map(a => println("\t" + a._1 + " -> " + a._2))
    println("\nThe probabilities of seeing each side of d_3 are: ")
    val probsAndSides3 = die3.probs zip die3.outcomes
    probsAndSides3.map(a => println("\t" + a._1 + " -> " + a._2))

    algorithm.kill
  }

}  

