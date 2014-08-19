/*
 * SmokersTest.scala  
 * Friends and smokes example tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
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
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example
import com.cra.figaro.test.tags.NonDeterministic

class SmokersTest extends WordSpec with Matchers {
  "The friends and smokers example" should {
    "produce the correct answer under importance sampling" taggedAs (Example, NonDeterministic)  in {
      test((e: Element[Boolean]) => Importance(20000, e))
    }

    "produce the correct answer under Metropolis-Hastings" taggedAs (Example, NonDeterministic) in {
      test((e: Element[Boolean]) => MetropolisHastings(50000, ProposalScheme.default, e))
    }

    "produce the correct answer under variable elimination" taggedAs (Example, NonDeterministic) in {
      test((e: Element[Boolean]) => VariableElimination(e))
    }
  }

  private class Person {
    val smokes = Flip(0.6)
  }

  private def smokingInfluence(pair: (Boolean, Boolean)) =
    if (pair._1 == pair._2) 3.0; else 1.0

  def test(algorithmCreator: Element[Boolean] => ProbQueryAlgorithm) {
    Universe.createNew()

    val alice, bob, clara = new Person
    val friends = List((alice, bob), (bob, clara))
    clara.smokes.observe(true)
    for { (p1, p2) <- friends } {
      ^^(p1.smokes, p2.smokes).setConstraint(smokingInfluence)
    }

    val pfft = 0.4 * 0.4 * 0.6 * 3.0 * 1.0
    val pftt = 0.4 * 0.6 * 0.6 * 1.0 * 3.0
    val ptft = 0.6 * 0.4 * 0.6 * 1.0 * 1.0
    val pttt = 0.6 * 0.6 * 0.6 * 3.0 * 3.0
    val answer = (ptft + pttt) / (pfft + pftt + ptft + pttt)

    val alg = algorithmCreator(alice.smokes)
    alg.start()
    alg.probability(alice.smokes, true) should be(answer +- 0.01)
    alg.kill
  }

}
