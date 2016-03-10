/*
 * CoinTosses.scala 
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

package com.cra.figaro.test.book.chap06

import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.sampling.Importance
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object CoinTosses {
  def main(args: Array[String]) {
    val outcomes = "10"
    val numTosses = outcomes.length

    val bias = Beta(2,5)
    val tosses = Array.fill(numTosses)(Flip(bias))
    val nextToss = Flip(bias)

    for {
      toss <- 0 until numTosses
    } {
      val outcome = outcomes(toss) == 'H'
      tosses(toss).observe(outcome)
    }

    val algorithm = Importance(nextToss, bias)
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    println("Average bias = " + algorithm.mean(bias))
    println("Probability of heads on next toss = " + algorithm.probability(nextToss, true))

    algorithm.kill()
  }
}

class CoinTossesTest extends WordSpec with Matchers {
  Universe.createNew()
  "Coin Tosses" should {
    val outcomes = "10"
    val numTosses = outcomes.length

    val bias = Beta(2,5)
    val tosses = Array.fill(numTosses)(Flip(bias))
    val nextToss = Flip(bias)

    for {
      toss <- 0 until numTosses
    } {
      val outcome = outcomes(toss) == 'H'
      tosses(toss).observe(outcome)
    }

    val algorithm = Importance(nextToss, bias)
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    val avgBias = algorithm.mean(bias)
    val pHeads = algorithm.probability(nextToss, true)

    algorithm.kill()

    "have an average bias equal to 0.222 +- 0.003" taggedAs (BookExample, NonDeterministic) in {
      avgBias should be(0.222 +- 0.003)
    }
    "have a probability of heads on next toss equal to 0.227 +- 0.003" taggedAs (BookExample, NonDeterministic) in {
      pHeads should be(0.227 +- 0.003)
    }
  }
}
