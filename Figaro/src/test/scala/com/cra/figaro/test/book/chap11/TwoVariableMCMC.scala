/*
 * TwoVariableMCMC.scala 
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

package com.cra.figaro.test.book.chap11

import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.compound.^^
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object TwoVariableMCMC {
  def main(args: Array[String]) {
    val x = Normal(0.75, 0.2)
    val y = Normal(0.4, 0.2)
    x.setCondition((d: Double) => d > 0 && d < 1)
    y.setCondition((d: Double) => d > 0 && d < 1)
    val pair = ^^(x, y)
    println(MetropolisHastings.probability(pair, (xy: (Double, Double)) => xy._1 > 0.5 && xy._2 > 0.5))
  }
}

class TwoVariableMCMCTest extends WordSpec with Matchers {
  Universe.createNew()
  val x = Normal(0.75, 0.2)
  val y = Normal(0.4, 0.2)
  x.setCondition((d: Double) => d > 0 && d < 1)
  y.setCondition((d: Double) => d > 0 && d < 1)
  val pair = ^^(x, y)
  val prob = MetropolisHastings.probability(pair, (xy: (Double, Double)) => xy._1 > 0.5 && xy._2 > 0.5)

  "Two Variable MCMC" should {
    "produce a probability = 0.283 +- 0.002" taggedAs (BookExample, NonDeterministic) in {
      prob should be(0.283 +- 0.002)
    }
  }
}
