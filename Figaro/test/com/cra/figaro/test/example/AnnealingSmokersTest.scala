/*
 * AnnealingSmokersTest.scala 
 * Bayesian network example tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.test.example

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound.^^
import com.cra.figaro.test._

class AnnealingSmokersTest extends WordSpec with ShouldMatchers {

  "A simple AnnealingSmokersTest" should {
    "produce the correct probability under Metropolis-Hastings Annealing" taggedAs (ExampleTest) in {

      Universe.createNew

      class Person {
        val smokes = Flip(0.6)
      }

      val alice, bob, clara = new Person
      val friends = List((alice, bob), (bob, clara))
      clara.smokes.observe(true)

      def smokingInfluence(pair: (Boolean, Boolean)) =
        if (pair._1 == pair._2) 3.0; else 1.0

      for { (p1, p2) <- friends } {
        ^^(p1.smokes, p2.smokes).setConstraint(smokingInfluence)
      }

      val alg = MetropolisHastingsAnnealer(ProposalScheme.default, Schedule.default(3.0))
      alg.start()
      Thread.sleep(1000)
      alg.stop()
      alg.mostLikelyValue(alice.smokes) should be(true)
      alg.mostLikelyValue(bob.smokes) should be(true)
      alg.kill
    }
  }

}