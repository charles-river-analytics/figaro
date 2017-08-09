/*
 * MultiDecisionMetropolisHastingsTest.scala 
 * Variable elimination tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.decision

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.decision._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import com.cra.figaro.util._
import com.cra.figaro.test.algorithm.decision.DecisionTestCases._
import com.cra.figaro.test._
import scala.collection.immutable.Map

class MultiDecisionMetropolisHastingsTest extends WordSpec with Matchers {

  def propmaker = (mv: Universe, e: Element[_]) => ProposalScheme.default(mv)

  "A MultiMetropolistHastings Sampler" when {

    "Running MultiMetropolisHastings" should {

      "produce the correct strategy with discrete parents" in {
        val (alg, declist, before, after) = MultiDecisionDiscrete((e1: List[Element[Double]], e2: List[Decision[Boolean, Double]]) =>
          MultiDecisionMetropolisHastings(20000, propmaker, 1000, e1, e2: _*), false)

        val d1 = declist(0)
        val d2 = declist(1)

        d1.getPolicy(true).value should be(0.1)
        d2.getPolicy(true).value should be(0.5)
        d1.getPolicy(false).value should be(0.5)
        d2.getPolicy(false).value should be(0.1)

      }

      "increase the expected value" in {
        val (alg, declist, before, after) = MultiDecisionDiscrete((e1: List[Element[Double]], e2: List[Decision[Boolean, Double]]) =>
          MultiDecisionMetropolisHastings(20000, propmaker, 1000, e1, e2: _*), true)
        after should be > (before)
      }

      "produce the correct strategy with continuous parents" in {
        val (alg, declist, before, after) = MultiDecisionContinuous((e1: List[Element[Double]], e2: List[Decision[Double, Double]]) => {
          def propmakerCont = (mv: Universe, e: Element[_]) => {
            UntypedScheme(() => e, Some(ProposalScheme.default))
          }
          MultiDecisionMetropolisHastings(50000, propmakerCont, 5000, e1, e2: _*)
        }, false)

        val d1 = declist(0)
        val d2 = declist(1)

        d1.getPolicy(1.0).value should be(0.1)
        d2.getPolicy(1.0).value should be(0.5)
        d1.getPolicy(0.0).value should be(0.5)
        d2.getPolicy(0.0).value should be(0.1)

      }

    }

  }

}
  
  
  
  
