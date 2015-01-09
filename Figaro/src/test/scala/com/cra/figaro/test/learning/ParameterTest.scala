/*
 * ParametizedTest.scala    
 * Tests for learnable parameters.
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

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._

class ParameterTest extends WordSpec with Matchers {
  "A Beta parameter" should {

    "not forget its sufficient statistics when placed in apply" in {
      val b = DirichletParameter(5, 10)
      class D {
        val l = List(0, 1)
      }
      val n = new D
      val f1 = Select(b, n.l: _*)
      val f2 = Select(b, n.l: _*)
      val f3 = Select(b, n.l: _*)
      val f4 = Select(b, n.l: _*)
      val f5 = Select(b, n.l: _*)

      val sum1 = (i1: Int, i2: Int) => i1 + i2
      val a1 = Apply(f1, f2, sum1)

      val sum2 = (i1: Int, i2: Int) => i1 + i2
      val a2 = Apply(f3, a1, sum2)
      val sum3 = (i1: Int, i2: Int) => i1 + i2
      val a3 = Apply(f4, a2, sum3)
      val sum4 = (i1: Int, i2: Int) => i1 + i2
      val a4 = Apply(f5, a3, sum4)
      a4.addCondition((i: Int) => (i >= 4))
      val numberOfIterations = 2

      val algorithm = EMWithVE(numberOfIterations, b)
      algorithm.start
    }

    "properly calculate expected value" in {
      val b = BetaParameter(1, 1)
      b.expectedValue should equal(0.5)
      val b2 = BetaParameter(3, 2)
      b2.expectedValue should be(0.6 +- 0.001)
    }

    "properly calculate MAP value" in {
      val b = BetaParameter(1, 1)
      b.MAPValue should equal(0.5)
      val b2 = BetaParameter(3, 2)
      b2.MAPValue should be((2.0 / 3.0) +- 0.001)
    }
    
    "properly maximize its alpha and beta hyperparameters" in {
      val b = BetaParameter(3, 2)
      b.maximize(Seq(1.0, 1.0))

      b.learnedAlpha should equal(4)
      b.learnedBeta should equal(3)

      b.maximize(Seq(3.0, 2.0))
      b.learnedAlpha should equal(6)
      b.learnedBeta should equal(4)

      b.maximize(Seq(0.0, 0.0))
      b.learnedAlpha should equal(3)
      b.learnedBeta should equal(2)

      b.maximize(Seq(0.5, 1.3))
      b.learnedAlpha should equal(3.5)
      b.learnedBeta should equal(3.3)
    }

    "provide the correct format of its sufficient statistics vector" in {
      val b = BetaParameter(3, 2)
      val s = b.zeroSufficientStatistics
      s.length should equal(2)
      for (entry <- s) {
        entry should equal(0.0)
      }
    }
  }

  "A Dirichlet parameter" should {
    "properly calculate expected value" in {
      val d = DirichletParameter(1, 1)
      d.expectedValue(0) should equal(0.5)
      val d2 = DirichletParameter(3, 2)
      d2.expectedValue(0) should be(0.6 +- 0.001)
    }

    "properly calculate MAP value" in {
      val d = DirichletParameter(1, 1)
      d.MAPValue(0) should equal(0.5)

      val d2 = BetaParameter(3, 2)
      d2.MAPValue should be((2.0 / 3.0) +- 0.001)
    }

    "properly maximize its hyperparameters" in {
      val b = DirichletParameter(3, 2)
      b.maximize(Seq(1.0, 1.0))

      b.concentrationParameters(0) should equal(4)
      b.concentrationParameters(1) should equal(3)

      b.maximize(Seq(3.0, 2.0))
      b.concentrationParameters(0) should equal(6)
      b.concentrationParameters(1) should equal(4)

      b.maximize(Seq(0.0, 0.0))
      b.concentrationParameters(0) should equal(3)
      b.concentrationParameters(1) should equal(2)

      b.maximize(Seq(0.5, 1.3))
      b.concentrationParameters(0) should equal(3.5)
      b.concentrationParameters(1) should equal(3.3)
    }

    "provide the correct format of its sufficient statistics vector" in {
      val d = DirichletParameter(3, 2)
      val s = d.zeroSufficientStatistics
      s.length should equal(2)
      for (entry <- s) {
        entry should equal(0.0)
      }
    }
  }

}
