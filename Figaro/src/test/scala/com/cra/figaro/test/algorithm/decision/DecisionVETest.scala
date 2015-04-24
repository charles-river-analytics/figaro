/*
 * DecisionVETest.scala  
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
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.library.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.util._
import com.cra.figaro.test.algorithm.decision.DecisionTestCases._
import scala.collection.mutable.Map

class DecisionVETest extends WordSpec with Matchers {

  "A VEGraph" when {

    "initially" should {
      "throw IllegalArgumentException when utilities are not doubles" in {
        Universe.createNew()
        val r1 = Flip(0.5)
        val d1 = CachingDecision[Boolean, Boolean](r1, List(true, false))
        val u1 = If(d1, Constant(true), Constant(false))
        an [IllegalArgumentException] should be thrownBy { DecisionVariableElimination(List(u1), d1) } 
      }
    }

    "computing the variables to eliminate" should {
      "include the decision variable and its parents" in {
        Universe.createNew()
        val r1 = Flip(0.5)
        val d1 = CachingDecision[Boolean, Boolean](r1, List(true, false))
        val u1 = If(d1, Constant(1.0), Constant(0.0))
        val ve = DecisionVariableElimination(List(u1), d1)
        ve.targetElements should equal(List(d1, r1))
      }
    }

    "Running VariableElimination" should {
      "produce the correct strategy" in {
        val (d, alg) = DecisionDiscrete((e1: List[Element[Double]], e2: Decision[Int, Boolean]) => DecisionVariableElimination(e1, e2))
        val D1 = d.getPolicy(-2)
        val D2 = d.getPolicy(2)
        val D3 = d.getPolicy(0)
        D1.value should equal(false)
        D2.value should equal(true)
        D3.value should equal(false)
      }

      "replace the old strategy" in {
        val (d, alg) = DecisionDiscrete((e1: List[Element[Double]], e2: Decision[Int, Boolean]) => DecisionVariableElimination(e1, e2))
        val v = VariableElimination(d)
        v.start()
        v.probability(d, true) should equal(.75 * .5)
        v.probability(d, false) should equal(1 - .75 * .5)
      }
    }

  }

}
  
  
  
  
