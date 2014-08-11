/*
 * ValveReliabilityTest.scala  
 * Dynamic Bayesian network example tests.
 * 
 * Created By:      William Kretschmer (kretsch@mit.edu), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jul 22, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.Example
import com.cra.figaro.language._
import com.cra.figaro.algorithm.filtering.FactoredFrontier

class ValveReliabilityTest extends WordSpec with Matchers {
  "A simple ValveReliabilityTest" should {
    "produce the correct probability using Factored Frontier" taggedAs (Example) in {
      trait ValveState
      case object OK extends ValveState
      case object RO extends ValveState
      case object RC extends ValveState

      val initial = Universe.createNew()
      Constant(OK)("v1", initial)
      Constant(OK)("v2", initial)
      Constant(OK)("v3", initial)

      val static = Universe.createNew()
      Select(0.997 -> OK, 0.002 -> RO, 0.001 -> RC)("f1", static)
      Select(0.995 -> OK, 0.003 -> RO, 0.002 -> RC)("f2", static)
      Select(0.993 -> OK, 0.004 -> RO, 0.003 -> RC)("f3", static)

      val failure = (v: ValveState, f: ValveState) => if (v == OK) f else v

      val transition = (static: Universe, previous: Universe) => {
        val v1prev = previous.getElementByReference[ValveState]("v1")
        val v2prev = previous.getElementByReference[ValveState]("v2")
        val v3prev = previous.getElementByReference[ValveState]("v3")
        val f1 = static.getElementByReference[ValveState]("f1")
        val f2 = static.getElementByReference[ValveState]("f2")
        val f3 = static.getElementByReference[ValveState]("f3")

        val next = Universe.createNew()
        val v1 = Apply(v1prev, f1, failure)("v1", next)
        val v2 = Apply(v2prev, f2, failure)("v2", next)
        val v3 = Apply(v3prev, f3, failure)("v3", next)

        val open = Apply(v1, v2, v3, (v1: ValveState, v2: ValveState, v3: ValveState) => v1 == RO && (v2 == RO || v3 == RO))("open", next)
        val closed = Apply(v1, v2, v3, (v1: ValveState, v2: ValveState, v3: ValveState) => v1 == RC || (v2 == RC && v3 == RC))("closed", next)
        val controllable = Apply(v1, v2, v3, (v1: ValveState, v2: ValveState, v3: ValveState) => !((v1 == RO && (v2 == RO || v3 == RO)) || (v1 == RC || (v2 == RC && v3 == RC))))("controllable", next)
        next
      }
      
      
      val ff = FactoredFrontier(static, initial, transition, 5)
      ff.start()
      for(_ <- 1 to 500) ff.advanceTime()
      ff.currentProbability("open", true) should be(0.42 +- 0.01)
      ff.currentProbability("closed", true) should be(0.37 +- 0.01)
      ff.currentProbability("controllable", true) should be(0.21 +- 0.01)
      ff.kill()
    }
  }
}