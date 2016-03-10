/*
 * ProbabilityOfEvidence.scala 
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
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.algorithm.factored.beliefpropagation.ProbEvidenceBeliefPropagation
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object ProbabilityOfEvidence {
  val pixels = Array.tabulate(4, 4)((i: Int, j: Int) => Flip(0.4)("pixel(" + i + "," + j + ")", Universe.universe))

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

  def makeNamedEvidence(i: Int, j: Int, obs: Boolean) =
    NamedEvidence("pixel(" + i + "," + j + ")", Observation(true))
  val evidence =
    List(makeNamedEvidence(0, 0, true),
         makeNamedEvidence(0, 2, false),
         makeNamedEvidence(1, 1, true),
         makeNamedEvidence(2, 0, true),
         makeNamedEvidence(2, 3, false),
         makeNamedEvidence(3, 1, true))

  def main(args: Array[String]) {
    println("Probability of evidence sampling")
    println(ProbEvidenceSampler.computeProbEvidence(100000, evidence))
    println("Probability of evidence belief propagation")
    println(ProbEvidenceBeliefPropagation.computeProbEvidence(20, evidence))
  }
}

class ProbabilityOfEvidenceTest extends WordSpec with Matchers {
  Universe.createNew()
  "Probability of Evidence" should {
    "produce a probability of evidence sampling = 0.002 +- 0.001" taggedAs (BookExample, NonDeterministic) in {
      ProbEvidenceSampler.computeProbEvidence(100000, ProbabilityOfEvidence.evidence) should be (0.002 +- 0.001)
    }
    "produce a probability of evidence belief propagation = 4.676522804 +- 0.000000001" taggedAs (BookExample, NonDeterministic) in {
      ProbEvidenceBeliefPropagation.computeProbEvidence(20, ProbabilityOfEvidence.evidence) should be(4.676522804 +- 0.000000001)
    }
  }
}
