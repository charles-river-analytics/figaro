/*
 * DecisionImportanceTest.scala 
 * Variable elimination tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import com.cra.figaro.test._
import com.cra.figaro.test.algorithm.decision.DecisionTestCases._
import scala.collection.mutable.Map
import com.cra.figaro.test.tags.NonDeterministic

class DecisionImportanceTest extends WordSpec with Matchers {

  "An Importance Sampler" when {

    "initially" should {

      "throw IllegalArgumentException when utilities are not doubles" in {
        Universe.createNew()
        val r1 = Flip(0.5)
        val d1 = Decision[Boolean, Boolean](r1, List(true, false))
        val u1 = If(d1, Constant(true), Constant(false))
        an [IllegalArgumentException] should be thrownBy { DecisionImportance(2000, List(u1), d1) } 
      }

    }

    "Running Importance Sampling with discrete decisons" should {
      "throw ParentValueNotFoundException if decision not encountered" in {
        val d = DecisionDiscrete((e1: List[Element[Double]], e2: Decision[Int, Boolean]) => DecisionImportance(10000, e1, e2))
        an [ParentValueNotFoundException] should be thrownBy { d._1.getPolicy(1) } 
      }

      "produce the correct strategy with discrete strategies" taggedAs(NonDeterministic) in {
        val (d, alg) = DecisionDiscrete((e1: List[Element[Double]], e2: Decision[Int, Boolean]) => DecisionImportance(20000, e1, e2))
        val D1 = d.getPolicy(-2)
        val D2 = d.getPolicy(0)
        val D3 = d.getPolicy(2)
        D1.value should equal(false)
        D2.value should equal(false)
        D3.value should equal(true)
        alg.getUtility(0, false).norm should be(0.0 +- 0.1)

      }

      "replace the old strategy" in {
        val (d, alg) = DecisionDiscrete((e1: List[Element[Double]], e2: Decision[Int, Boolean]) => DecisionImportance(20000, e1, e2))
        val v = Importance(5000, d)
        v.start()
        v.probability(d, true) should be(.75 * .5 +- .02)
        v.probability(d, false) should be(1 - .75 * .5 +- .02)
      }

      "produce the correct strategy with discrete strategies and posted evidence" in {
        val (d, alg) = DecisionDiscreteEvidence((e1: List[Element[Double]], e2: Decision[Int, Boolean]) => DecisionImportance(30000, e1, e2))
        val D1 = d.getPolicy(-2)
        val D2 = d.getPolicy(0)
        val D3 = d.getPolicy(2)
        D1.value should equal(false)
        D2.value should equal(false)
        D3.value should equal(true)
        alg.getUtility(0, false).norm should be(0.0 +- 0.1)
      }

    }

    "Running Importance Sampling with continuous decisons" should {
      "produce the correct strategy with continuous strategies" in {
        val d = DecisionContinuous((e1: List[Element[Double]], e2: Decision[Double, Boolean]) => DecisionImportance(30000, e1, e2), false)._1
        val D1 = d.getPolicy(-2)
        val D2 = d.getPolicy(0)
        val D3 = d.getPolicy(2)
        D1.value should equal(false)
        D2.value should equal(false)
        D3.value should equal(true)
        val u = d.policy.toUtility()(0).value
        u should be(0.0 +- 0.20)
      }

      "increase the expected utility after optimization" in {
        val all = DecisionContinuous((e1: List[Element[Double]], e2: Decision[Double, Boolean]) => DecisionImportance(30000, e1, e2), true)
        all._4 should be > (all._3)
      }

      "produce the correct strategy with continuous strategies and posted evidence" in {
        val d = DecisionContinuousEvidence((e1: List[Element[Double]], e2: Decision[Double, Boolean]) => DecisionImportance(35000, e1, e2), false)._1
        val D1 = d.getPolicy(-2)
        val D2 = d.getPolicy(0)
        val D3 = d.getPolicy(2)
        D1.value should equal(false)
        D2.value should equal(false)
        D3.value should equal(true)
        val u = d.policy.toUtility()(0).value
        u should be(0.0 +- 0.20)
      }
    }

  }

}
  
  
  
  
