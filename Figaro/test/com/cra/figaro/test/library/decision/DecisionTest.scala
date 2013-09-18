/*
 * DecisionTest.scala  
 * Variable elimination tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.test.library.decision

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.decision._

class DecisionTest extends WordSpec with ShouldMatchers {

  "A CachingDecision" should {

    "expand all parent values" in {
      val U = Universe.createNew()
      val u1 = Uniform((0 until 5): _*)
      val d1 = CachingDecision(u1, 5 until 10)
      val elems = Values()(u1).map(v => d1.get(v))
      elems.foreach(s => assert(U.activeElements contains s))
    }

    "remove all cached elements when setting the strategy" in {
      val U = Universe.createNew()
      val u1 = Uniform((0 until 5): _*)
      val d1 = CachingDecision(u1, 5 until 10)
      val elems = Values()(u1).map(v => d1.get(v))
      d1.setPolicy((i: Int) => Constant(i))
      elems.foreach(s => U.uses(d1) should not contain (s))
      elems.foreach(_.active should be(false))
    }

  }

  "A NonCachingDecision" should {

    "remove previous elements when parent value changes" in {
      val U = Universe.createNew()
      val u1 = Uniform((0 until 5): _*)
      val d1 = NonCachingDecision(u1, 5 until 10)
      d1.setPolicy((i: Int) => Constant(i))
      val prev = d1.get(u1.value)
      val curr = d1.get((u1.value + 1) % 5)
      U.uses(d1) should not contain (prev)
      prev.active should be(false)
    }

  }

}













