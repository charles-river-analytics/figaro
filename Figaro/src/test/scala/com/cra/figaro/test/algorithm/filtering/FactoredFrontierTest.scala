/*
 * FactoredFrontierTest.scala  
 * Tests for the Factored Frontier Algorithm.
 * 
 * Created By:      William Kretschmer (kretsch@mit.edu), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jul 16, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.filtering

import org.scalatest.Matchers
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.language.Flip
import com.cra.figaro.language.Select
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.filtering.FactoredFrontier
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.algorithm.lazyfactored.LazyValues

class FactoredFrontierTest extends WordSpec with Matchers {
  "A factored frontier" when {
    "creating a new universe from a previous time step" should {
      "create dummy universes for the previous and static universes" in {
        val universe1 = createNew()
        val static1 = createNew()
        def trans(static: Universe, previous: Universe): Universe = {
          static should not equal static1
          previous should not equal universe1
          createNew()
        }
        val ff = FactoredFrontier(static1, universe1, trans(_, _), 1)
        ff.start()
        ff.advanceTime()
        ff.kill()
      }
      
      "copy all named elements from the previous universe as atomic selects of the appropriate type" in {
        val universe1 = createNew()
        val f1 = Flip(0.2)("f1", universe1)
        val f2 = If(f1, 0.3, 0.6)("f2", universe1)
        val f3 = Flip(f2)("f3", universe1)
        val f4 = If(f1 === f3, 0, 1)("f4", universe1)
        def trans(u: Universe): Universe = {
          u.getElementByReference[Boolean]("f1") shouldBe an [AtomicSelect[Boolean]]
          u.getElementByReference[Double]("f2") shouldBe an [AtomicSelect[Double]]
          u.getElementByReference[Boolean]("f3") shouldBe an [AtomicSelect[Boolean]]
          u.getElementByReference[Int]("f4") shouldBe an [AtomicSelect[Int]]
          createNew()
        }
        val ff = FactoredFrontier(universe1, trans(_), 3)
        ff.start()
        ff.advanceTime()
        ff.kill()
      }
      
      "copy all named elements from the previous static universe as atomic selects of the appropriate type" in {
        val static = createNew()
        val f1 = Flip(0.2)("f1", static)
        val f2 = If(f1, 0.3, 0.6)("f2", static)
        val universe1 = createNew()
        val f3 = Flip(f2)("f3", universe1)
        val f4 = If(f1 === f3, 0, 1)("f4", universe1)
        def trans(static: Universe, previous: Universe): Universe = {
          static.getElementByReference[Boolean]("f1") shouldBe an [AtomicSelect[Boolean]]
          static.getElementByReference[Double]("f2") shouldBe an [AtomicSelect[Double]]
          previous.getElementByReference[Boolean]("f3") shouldBe an [AtomicSelect[Boolean]]
          previous.getElementByReference[Int]("f4") shouldBe an [AtomicSelect[Int]]
          previous.hasRef[Boolean]("f1") should equal(false)
          previous.hasRef[Double]("f2") should equal(false)
          static.hasRef[Boolean]("f3") should equal(false)
          static.hasRef[Int]("f4") should equal(false)
          createNew()
        }
        val ff = FactoredFrontier(static, universe1, trans(_, _), 3)
        ff.start()
        ff.advanceTime()
        ff.kill()
      }
      
      "clear the LazyValues associated with the previous universe and static universe" in {
        val static1 = createNew()
        val f1 = Select(0.1 -> 0.2, 0.4 -> 0.3, 0.5 -> 0.6)("f1", static1)
        val universe1 = createNew()
        val f2 = Flip(f1)("f2", universe1)
        def trans(static: Universe, previous: Universe): Universe = {
          LazyValues(static1).storedValues(f1).xvalues should be(empty)
          LazyValues(universe1).storedValues(f2).xvalues should be(empty)
          createNew()
        }
        val ff = FactoredFrontier(static1, universe1, trans(_, _), 3)
        ff.start()
        LazyValues(static1).storedValues(f1).xvalues should not be empty
        LazyValues(universe1).storedValues(f2).xvalues should not be empty
        ff.advanceTime()
        ff.kill()
      }
    }
    
    "constructing the initial belief state" should {
      "contain a state with fraction proportional to its probability" in {
        createNew()
        val f1 = Flip(0.2)("f1", universe)
        val i2 = If(f1, Flip(0.3), Flip(0.6))("f2", universe)
        i2.observe(true)
        val qf1True = 0.2 * 0.3
        val qf1False = 0.8 * 0.6
        val ff1True = qf1True / (qf1True + qf1False)
        val ff = FactoredFrontier(universe, (u: Universe) => new Universe(), 10)
        ff.start()
        ff.currentProbability("f1", true) should be(ff1True +- 0.02)
        ff.kill()
      }
    }

    "constructing a new belief state with no evidence" should {      
      "contain a state with fraction proportional to its expected probability under the initial belief state" in {
        val universe1 = createNew()
        val f1 = Flip(0.2)("f", universe1)
        def trans(u: Universe): Universe = {
          val universe2 = createNew()
          val f2 = If(u.getElementByReference[Boolean]("f"), Flip(0.8), Flip(0.3))("f", universe2)
          universe2
        }
        val ff = FactoredFrontier(universe1, trans(_), 10)
        ff.start()
        ff.advanceTime(List())
        val p = 0.2 * 0.8 + 0.8 * 0.3
        ff.currentProbability("f", true) should be(p +- 0.02)
        ff.kill()
      }
    }

    "constructing a new belief state with evidence" should {
      "contain a state with fraction proportional to its expected probability under the initial belief state" +
        "conditioned on the evidence" in {
          val universe1 = createNew()
          val f11 = Flip(0.2)("f1", universe1)
          def trans(u: Universe): Universe = {
            val universe2 = createNew()
            val f12 = If(u.getElementByReference[Boolean]("f1"), Flip(0.8), Flip(0.3))("f1", universe2)
            val f22 = If(f12, Flip(0.6), Flip(0.1))("f2", universe2)
            universe2
          }
          val ff = FactoredFrontier(universe1, trans(_), 10)
          ff.start()
          ff.advanceTime(List(NamedEvidence("f2", Observation(true))))
          val qf1True = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
          val qf1False = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
          val ff1True = qf1True / (qf1True + qf1False)
          ff.currentProbability("f1", true) should be(ff1True +- 0.02)
          ff.kill()
        }

      "correctly estimate static variables" in {
        val static = createNew()
        val x = Flip(0.2)("x", static)
        val universe2 = createNew()
        def trans(static: Universe, previous: Universe): Universe = {
          val universe3 = createNew()
          val y = If(static.getElementByReference[Boolean]("x"), Flip(0.8), Flip(0.1))("y", universe3)
          universe3
        }
        val ff = FactoredFrontier(static, universe2, trans(_, _), 10)
        ff.start()
        ff.advanceTime(List(NamedEvidence("y", Observation(true))))
        val qxTrue = 0.2 * 0.8
        val qxFalse = 0.8 * 0.1
        val pxTrue = qxTrue / (qxTrue + qxFalse)
        ff.currentProbability("x", true) should be(pxTrue +- 0.02)
        ff.kill()
      }
    }

    "iterating over two time steps with evidence on each step" should {
      "contain a state with the correct probability" in {
        val universe1 = createNew()
        Flip(0.2)("f1", universe1)
        def trans(previous: Universe): Universe = {
          val universe2 = createNew()
          val previousF1 = previous.getElementByReference[Boolean]("f1")
          val f1 = If(previousF1, Flip(0.8), Flip(0.3))("f1", universe2)
          val f2 = If(f1, Flip(0.6), Flip(0.1))("f2", universe2)
          universe2
        }
        val ff = FactoredFrontier(universe1, trans(_), 10)
        ff.start()
        ff.advanceTime(List(NamedEvidence("f2", Observation(true))))
        val result1 = ff.currentProbability("f1", true)
        ff.advanceTime(List(NamedEvidence("f2", Observation(false))))
        val result2 = ff.currentProbability("f1", true)
        val ff11T = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
        val ff11F = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
        val answer1 = ff11T / (ff11T + ff11F)
        val ff10T_F11T_F21T_F12T_F22F = 0.2 * 0.8 * 0.6 * 0.8 * 0.4
        val ff10T_F11T_F21T_F12F_F22F = 0.2 * 0.8 * 0.6 * 0.2 * 0.9
        val ff10T_F11F_F21T_F12T_F22F = 0.2 * 0.2 * 0.1 * 0.3 * 0.4
        val ff10T_F11F_F21T_F12F_F22F = 0.2 * 0.2 * 0.1 * 0.7 * 0.9
        val ff10F_F11T_F21T_F12T_F22F = 0.8 * 0.3 * 0.6 * 0.8 * 0.4
        val ff10F_F11T_F21T_F12F_F22F = 0.8 * 0.3 * 0.6 * 0.2 * 0.9
        val ff10F_F11F_F21T_F12T_F22F = 0.8 * 0.7 * 0.1 * 0.3 * 0.4
        val ff10F_F11F_F21T_F12F_F22F = 0.8 * 0.7 * 0.1 * 0.7 * 0.9
        val ff12T = ff10T_F11T_F21T_F12T_F22F + ff10T_F11F_F21T_F12T_F22F + ff10F_F11T_F21T_F12T_F22F + ff10F_F11F_F21T_F12T_F22F
        val ff12F = ff10T_F11T_F21T_F12F_F22F + ff10T_F11F_F21T_F12F_F22F + ff10F_F11T_F21T_F12F_F22F + ff10F_F11F_F21T_F12F_F22F
        result1 should be(answer1 +- 0.02)
        val answer2 = ff12T / (ff12T + ff12F)
        result2 should be(answer2 +- 0.02)
        ff.kill()
      }

      "correctly estimate static variables" in {
        val static = createNew()
        val x = Flip(0.2)("x", static)
        val initial = createNew()
        val y = Flip(0.3)("y", initial)
        def trans(static: Universe, previous: Universe): Universe = {
          val universe3 = createNew()
          val y = If(static.getElementByReference[Boolean]("x"), Flip(0.8), previous.getElementByReference[Boolean]("y"))("y", universe3)
          universe3
        }
        val ff = FactoredFrontier(static, initial, trans(_, _), 10)
        ff.start()
        ff.advanceTime(List(NamedEvidence("y", Observation(true))))
        val pXY0TT = 0.2 * 0.3
        val pXY0TF = 0.2 * 0.7
        val pXY0FT = 0.8 * 0.3
        val pXY0FF = 0.8 * 0.7
        val pYtTGivenXYtMinusTT = 0.8
        val pYtFGivenXYtMinusTT = 0.2
        val pYtTGivenXYtMinusTF = 0.8
        val pYtFGivenXYtMinusTF = 0.2
        val pYtTGivenXYtMinusFT = 1.0
        val pYtFGivenXYtMinusFT = 0.0
        val pYtTGivenXYtMinusFF = 0.0
        val pYtFGivenXYtMinusFF = 1.0
        val pY2TGivenXY0TT = pYtTGivenXYtMinusTT * pYtTGivenXYtMinusTT + pYtFGivenXYtMinusTT * pYtTGivenXYtMinusTF
        val pY2TGivenXY0TF = pYtTGivenXYtMinusTF * pYtTGivenXYtMinusTT + pYtFGivenXYtMinusTF * pYtTGivenXYtMinusTF
        val pY2TGivenXY0FT = pYtTGivenXYtMinusFT * pYtTGivenXYtMinusFT + pYtFGivenXYtMinusFT * pYtTGivenXYtMinusFF
        val pY2TGivenXY0FF = pYtTGivenXYtMinusFF * pYtTGivenXYtMinusFT + pYtFGivenXYtMinusFF * pYtTGivenXYtMinusFF
        val pXY2TT = pXY0TT * pY2TGivenXY0TT + pXY0TF * pY2TGivenXY0TF
        val pXY2FT = pXY0FT * pY2TGivenXY0FT + pXY0FF * pY2TGivenXY0FF
        val pxTrue = pXY2TT / (pXY2TT + pXY2FT)
        ff.currentProbability("x", true) should be(pxTrue +- 0.02)
        ff.kill()
      }
    }

    "iterating over two time steps with evidence on each step" should {
      "contain a the correct distribution over an element" in {
        val universe1 = createNew()
        Flip(0.2)("f1", universe1)
        def trans(previous: Universe): Universe = {
          val universe2 = createNew()
          val previousF1 = previous.getElementByReference[Boolean]("f1")
          val f1 = If(previousF1, Flip(0.8), Flip(0.3))("f1", universe2)
          val f2 = If(f1, Flip(0.6), Flip(0.1))("f2", universe2)
          universe2
        }
        val ff = FactoredFrontier(universe1, trans(_), 10)
        ff.start()
        ff.advanceTime(List(NamedEvidence("f2", Observation(true))))
        ff.advanceTime(List(NamedEvidence("f2", Observation(false))))
        val qf1TrueTime1 = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
        val qf1FalseTime1 = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
        val ff1TrueTime1 = qf1TrueTime1 / (qf1TrueTime1 + qf1FalseTime1)
        val ff1FalseTime1 = 1 - ff1TrueTime1
        val qf1TrueTime2 = (ff1TrueTime1 * 0.8 + ff1FalseTime1 * 0.3) * 0.4
        val qf1FalseTime2 = (ff1TrueTime1 * 0.2 + ff1FalseTime1 * 0.7) * 0.9
        val ff1TrueTime2 = qf1TrueTime2 / (qf1TrueTime2 + qf1FalseTime2)
        val d = ff.currentDistribution("f1").asInstanceOf[Stream[(Double, Boolean)]]
        d.size should equal(2)
        if (d(0)._2 == true) {
          d(0)._2 should equal(true)
          d(1)._2 should equal(false)
          d(0)._1 should be(ff1TrueTime2 +- 0.02)
          d(1)._1 should be((1.0 - ff1TrueTime2) +- 0.02)
        } else {
          d(1)._2 should equal(true)
          d(0)._2 should equal(false)
          d(1)._1 should be(ff1TrueTime2 +- 0.02)
          d(0)._1 should be((1.0 - ff1TrueTime2) +- 0.02)
        }
        ff.kill()
      }
    }

    "iterating over many time steps" should {
      "not suffer from memory leaks" taggedAs (Performance) in {
        class BooleanTest{
          var b = false
        }
        
        class UniverseTest(finalized: BooleanTest) extends Universe(List()){
          override def finalize(){
            super.finalize()
            finalized.b = true
          }
        }
        
        val numSteps = 1000
        val finalized = new BooleanTest
        val universe1 = createNew()
        Constant(Array.fill(1000)(0))("f1", universe1)
        def trans(previous: Universe): Universe = {
          // At least one of the previous universes should be finalized.
          // We always use the same transition function, so we assume that if one previous universe is finalized, all others should be finalized as well.
          val universe2 = if(finalized.b) createNew() else new UniverseTest(finalized)
          val previousF1 = previous.getElementByReference[Array[Int]]("f1")
          Apply(previousF1, (a: Array[Int]) => a.map(_ + 1))("f1", universe2)
          universe2
        }
        val ff = FactoredFrontier(universe1, trans(_), 10)
        ff.start()
        for { i <- 1 to numSteps } {
          ff.advanceTime(List())
        }
        ff.kill()
        System.gc()
        finalized.b should equal(true)
      }
    }
  }
}