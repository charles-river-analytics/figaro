/*
 * DecisionTest.scala  
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

package com.cra.figaro.test.library.decision

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.decision._

class DecisionTest extends WordSpec with Matchers {

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


}













