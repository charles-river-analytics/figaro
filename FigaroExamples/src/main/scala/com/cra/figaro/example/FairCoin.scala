/*
 * FairCoin.scala
 * A model for learning the fairness of a coin.
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
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.patterns.learning._
/**
 * A model for learning the fairness of a coin.
 */

class Trials(val parameters: ParameterCollection) {
  val trials = for (f <- 0 until 86) yield {
    Flip(parameters.get("fairness"))
  }
}

object CoinExample {

  Universe.createNew()

  /*
   * This is an easy way of representing 86 coin flips.
   * An 'H' represents 'heads' and 'T' represents tails.
   * There are 62 'H's and 24 'T's.
   */
  val data = Seq(
    'H', 'H', 'H', 'T', 'H', 'H', 'T', 'H', 'T', 'H', 'H', 'T', 'H',
    'T', 'H', 'H', 'T', 'H', 'T', 'T', 'H', 'T', 'H', 'H', 'T', 'H', 'H', 'H', 'T',
    'T', 'H', 'H', 'T', 'H', 'T', 'H', 'T', 'T', 'H', 'T', 'H', 'H', 'H', 'H', 'H',
    'H', 'H', 'H', 'H', 'T', 'H', 'T', 'H', 'H', 'T', 'H', 'H', 'H', 'H', 'H',
    'H', 'T', 'H', 'H', 'H', 'T', 'H', 'H', 'H', 'H', 'H', 'H', 'H',
    'H', 'H', 'H', 'H', 'H', 'H', 'H', 'T', 'H', 'T',
    'H', 'H', 'H')

  def main(args: Array[String]) {

    val params = ModelParameters()
    val fairness = Beta(2.0, 2.0)("fairness", params)
    val model = new Trials(params.priorParameters)
    data zip model.trials foreach {
      (datum: (Char, Flip)) => if (datum._1 == 'H') datum._2.observe(true) else datum._2.observe(false)
    }

    val numberOfEMIterations = 10
    val numberOfBPIterations = 10
    val learningAlgorithm = EMWithBP(10, 10, params)
    learningAlgorithm.start
    learningAlgorithm.stop
    learningAlgorithm.kill
    /*
     * This will create a flip having a probability of 'true' learned from the input data. 
     */
    println("The probability of a coin with this fairness showing 'heads' is: ")
    println(params.posteriorParameters.get("fairness"))

    val t1 = Flip(params.posteriorParameters.get("fairness"))
    val t2 = Flip(params.posteriorParameters.get("fairness"))

    val equal = t1 === t2

    val alg = VariableElimination(equal)
    alg.start()
    println("The probability of two coins which exhibit this fairness showing the same side is: " + alg.probability(equal, true))
    alg.kill()

  }

}  

