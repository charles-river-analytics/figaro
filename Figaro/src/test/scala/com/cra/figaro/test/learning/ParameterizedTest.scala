/*
 * ParameterizedTest.scala   
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
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec

import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.atomic.continuous._

class ParameterizedTest extends WordSpec with PrivateMethodTester with Matchers {
  "A flip" when {
    "created from a beta parameter" should {
      "create a parameterized flip" in {
        val b = Beta(5, 2)
        val f = Flip(b)
        f.isInstanceOf[ParameterizedFlip] should equal(true)
      }
    }
    
   "converting a distribution to sufficient statistics" should {
      "return zero statistics if not associated with the input parameter" in {
        val b1 = Beta(5, 2)
        val b2 = Beta(5, 2)
        val f = Flip(b2)
        val statistics = f match {
          case pf: SingleParameterized[Boolean] => pf.distributionToStatistics(b1,List((0.50,true),(0.50,false)).toStream)
          case _ => Seq.empty[Double]
        }
        statistics.isEmpty should equal(false)
        statistics(0) should equal (0.0)
        statistics(1) should equal (0.0)
      }
     
      "return statistics if associated with the input parameter" in {
        val b1 = Beta(5, 2)
        val b2 = Beta(5, 2)
        val f = Flip(b2)
        val statistics = f match {
          case pf: SingleParameterized[Boolean] => pf.distributionToStatistics(b2,List((0.50,true),(0.50,false)).toStream)
          case _ => Seq.empty[Double]
        }
        statistics.isEmpty should equal(false)
      }
    }
  }
}
