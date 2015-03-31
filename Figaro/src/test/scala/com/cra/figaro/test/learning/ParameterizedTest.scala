/*
 * ParametizedTest.scala   
 * Tests for parameterized elements.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.learning

import org.scalatest.Matchers
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._

class ParameterizedTest extends WordSpec with PrivateMethodTester with Matchers {
  "A flip" when {
    "created from a beta parameter" should {
      "create a parameterized flip" in {
        val b = Beta(5, 2)
        val f = Flip(b)
        f.isInstanceOf[ParameterizedFlip] should equal(true)
      }
    }
  }
}
