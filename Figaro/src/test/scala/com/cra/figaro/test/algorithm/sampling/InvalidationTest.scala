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

import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.language.Chain
import com.cra.figaro.language.Flip
import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.discrete.Uniform

class InvalidationTest extends WordSpec with Matchers with PrivateMethodTester {
  "Invalidating MetropolisHastings" should {

    "calculate constrained element value used by an invalidated element" in {

      for (i <- 0 until 10) {
        val universe = Universe.createNew
        val p = Uniform(0, 1)("p", universe)
        val f1 = Flip(.3)("f1", universe)
        val f2 = Flip(.8)("f2", universe)
        val q = Chain(p, (d: Int) => {
          if (d > 0) f1 else f2
        })("q", universe)

        q.observe(true)

        val targets = List(p, f1, f2, q)
        val alg = MetropolisHastings(200000, ProposalScheme.default, targets: _*)
        alg.start
        alg.stop
        alg.probability(q, true) should equal(1.0)
        alg.kill
      }

    }

  }

}