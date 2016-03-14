/*
 * MPE.scala 
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

package com.cra.figaro.test.book.chap12

import com.cra.figaro.language._
import com.cra.figaro.library.compound.^^
import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.sampling.MetropolisHastingsAnnealer
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.algorithm.sampling.Schedule
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object MPE {
  val pixels = Array.fill(4, 4)(Flip(0.4))

  def makeConstraint(pixel1: Element[Boolean], pixel2: Element[Boolean]) {
    val pairElem = ^^(pixel1, pixel2)
    pairElem.setConstraint(pair => if (pair._1 == pair._2) 1.0 else 0.5)
  }

  for {
    i <- 0 until 4
    j <- 0 until 4
  } {
    if (i > 0) makeConstraint(pixels(i-1)(j), pixels(i)(j))
    if (j > 0) makeConstraint(pixels(i)(j-1), pixels(i)(j))
  }

  pixels(0)(0).observe(true)
  pixels(0)(2).observe(false)
  pixels(1)(1).observe(true)
  pixels(2)(0).observe(true)
  pixels(2)(3).observe(false)
  pixels(3)(1).observe(true)

  def run(algorithm: OneTimeMPE) {
    algorithm.start()
    for { i <- 0 until 4 } {
      for { j <- 0 until 4 } {
        print(algorithm.mostLikelyValue(pixels(i)(j)))
        print("\t")
      }
      println()
    }
    println()
    algorithm.kill()
  }

  def main(args: Array[String]) {
    println("MPE variable elimination")
    run(MPEVariableElimination())
    println("MPE belief propagation")
    run(MPEBeliefPropagation(10))
    println("Simulated annealing")
    run(MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)))
  }
}

class MPETest extends WordSpec with Matchers {
  Universe.createNew()
  "MPE" should {
    "produce the correct result with Variable Elimination" taggedAs (BookExample, NonDeterministic) in {
      MPE.run(MPEVariableElimination())
    }
    "produce the correct result with Belief Propagation" taggedAs (BookExample, NonDeterministic) in {
      MPE.run(MPEBeliefPropagation(10))
    }
    "produce the correct result with Simulated Annealing" taggedAs (BookExample, NonDeterministic) in {
      MPE.run(MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)))
    }
  }
}
