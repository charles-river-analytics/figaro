/*
 * InvalidationTest.scala
 * 
 * Created By:      Alison O'Connor (aoconnor@cra.com)
 * Creation Date:   August 15, 2017
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.sampling

import scala.language.existentials

import org.scalatest.Matchers
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec

import com.cra.figaro.test._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Chain
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Select
import com.cra.figaro.language.Apply

class InvalidationTest extends WordSpec with Matchers with PrivateMethodTester {
  "Invalidating MetropolisHastings" should {

    "perform calculation correctly for an invalidated element" in {
      val universe = Universe.createNew

      val p = Uniform(1, 2, 3)("p", universe)
      val f1 = Flip(.5)("f1", universe)
      val f2 = Flip(.8)("f2", universe)

      val q = Chain(p, (d: Int) => {
        if (d > 2) Chain(f1, (b: Boolean) => {
          if (b) Flip(0.7) else Flip(0.3)
        })
        else {
          Chain(f2, (b: Boolean) => {
            if (b) Flip(0.2) else Flip(0.4)
          })
        }
      })("q", universe)

      val w1 = Apply(f1, (b: Boolean) => if (b) false else true)
      val w2 = Apply(f2, (b: Boolean) => if (b) false else true)
      val w3 = Apply(q, (b: Boolean) => if (b) false else true)

      val result = (1.0 / 3.0 * 0.5) + (2.0 / 3.0 * 0.8)

      val targets = List(p, f1, f2, q, w1, w2, w3)
      val alg = MetropolisHastings(200000, ProposalScheme.default, targets: _*)
      alg.start
      alg.stop
      for (t <- List(p)) println(alg.expectation(t, (d: Int) => d))
      for (t <- List(f1, f2, q, w1, w2, w3)) println(alg.probability(t, true))
      println

      val alg2 = MetropolisHastingsAmo(200000, ProposalScheme.default, targets: _*)
      alg2.start
      alg2.stop
      for (t <- List(p)) println(alg2.expectation(t, (d: Int) => d))
      for (t <- List(f1, f2, q, w1, w2, w3)) println(alg2.probability(t, true))
      println

      val alg3 = VariableElimination(targets: _*)
      alg3.start
      alg3.stop
      for (t <- List(p)) println(alg3.expectation(t, (d: Int) => d))
      for (t <- List(f1, f2, q, w1, w2, w3)) println(alg3.probability(t, true))
      println
    }

  }

}