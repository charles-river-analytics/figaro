/*
 * ParticleFilterTest.scala 
 * Particle filter tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.filtering

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import com.cra.figaro.algorithm.filtering._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.compound._
import com.cra.figaro.test._
import scala.language.existentials
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic

class ParticleFilterTest extends WordSpec with PrivateMethodTester with Matchers {
  "A snapshot" should {
    "contain the values of all named elements" in {
      val universe1 = Universe.createNew()
      val x = Flip(0.2)("x", universe1)
      val y = Select(0.3 -> 1, 0.5 -> 2, 0.2 -> 3)("y", universe1)
      x.value = false
      y.value = 3
      val snapshot = new Snapshot
      snapshot.store(universe1)
      x.value = true
      y.value = 2
      snapshot(x) should equal(false)
      snapshot(y) should equal(3)
    }

    "restore the state of the original universe" in {
      val universe1 = Universe.createNew()
      val x = Flip(0.2)("x", universe1)
      val y = Select(0.3 -> 1, 0.5 -> 2, 0.2 -> 3)("y", universe1)
      x.value = false
      y.value = 3
      val snapshot = new Snapshot
      snapshot.store(universe1)
      val universe2 = Universe.createNew()
      val x2 = Flip(0.7)("x", universe2)
      val y2 = Flip(0.9)("y", universe2)
      x2.value = true
      snapshot.restore(universe2)
      x2.value should equal(false)
    }
  }

  "A particle filter" when {
    "constructing a belief state from a set of weighted samples" should {
      "contain a sample with fraction proportional to its weight" in {
        val staticSnapshot = new Snapshot
        createNew()
        val numParticles = 10000
        val f1 = Flip(0.2)("f1", universe)
        val f2 = Flip(0.7)("f2", universe)
        f1.value = true
        f2.value = false
        val dynamicSnapshot1 = new Snapshot
        dynamicSnapshot1.store(universe)
        val state1 = new State(dynamicSnapshot1, staticSnapshot)
        f1.value = false
        f2.value = false
        val dynamicSnapshot2 = new Snapshot
        dynamicSnapshot2.store(universe)
        val state2 = new State(dynamicSnapshot2, staticSnapshot)
        val pf = ParticleFilter(universe, (u: Universe) => new Universe(), numParticles)
        val pfClass = pf.getClass.getSuperclass
        val weightedParticles: Seq[ParticleFilter.WeightedParticle] = List((0.4, state1), (0.6, state2))

        pf.updateBeliefState(weightedParticles)
        (pf.beliefState count (_ == state1)).toDouble / numParticles should be(0.4 +- 0.01)
      }
    }

    "constructing the initial belief state" should {
      "contain a state with fraction proportional to its probability" taggedAs(NonDeterministic) in {
        createNew()
        val numParticles = 20000
        val f1 = Flip(0.2)("f1", universe)
        val i2 = If(f1, Flip(0.3), Flip(0.6))("f2", universe)
        i2.observe(true)
        val qf1True = 0.2 * 0.3
        val qf1False = 0.8 * 0.6
        val pf1True = qf1True / (qf1True + qf1False)
        val pf = ParticleFilter(universe, (u: Universe) => new Universe(), numParticles)
        pf.start()
        pf.currentProbability("f1", true) should be(pf1True +- 0.01)
      }
    }

    "constructing a new belief state with no evidence" should {
      "contain a state with fraction proportional to its expected probability under the initial belief state" in {
        val numParticles = 20000
        val universe1 = createNew()
        val f1 = Flip(0.2)("f", universe1)
        def trans(u: Universe): Universe = {
          val universe2 = createNew()
          val f2 = If(u.get[Boolean]("f"), Flip(0.8), Flip(0.3))("f", universe2)
          universe2
        }
        val pf = ParticleFilter(universe1, trans, numParticles)
        pf.start()
        pf.advanceTime(List())
        val p = 0.2 * 0.8 + 0.8 * 0.3
        pf.currentProbability("f", true) should be(p +- 0.01)
      }
    }

    "constructing a new belief state with evidence" should {
      "contain a state with fraction proportional to its expected probability under the initial belief state" +
        "conditioned on the evidence" in {
          val numParticles = 50000
          val universe1 = createNew()
          val f11 = Flip(0.2)("f1", universe1)
          def trans(u: Universe): Universe = {
            val universe2 = createNew()
            val f12 = If(u.get[Boolean]("f1"), Flip(0.8), Flip(0.3))("f1", universe2)
            val f22 = If(f12, Flip(0.6), Flip(0.1))("f2", universe2)
            universe2
          }
          val pf = ParticleFilter(universe1, trans, numParticles)
          pf.start()
          pf.advanceTime(List(NamedEvidence("f2", Observation(true))))
          val qf1True = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
          val qf1False = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
          val pf1True = qf1True / (qf1True + qf1False)
          pf.currentProbability("f1", true) should be(pf1True +- 0.01)
        }

      "correctly estimate static variables" in {
        val numParticles = 100000
        val static = createNew()
        val x = Flip(0.2)("x", static)
        val universe2 = createNew()
        def trans(static: Universe, previous: Universe): Universe = {
          val universe3 = createNew()
          val y = If(static.get[Boolean]("x"), Flip(0.8), Flip(0.1))("y", universe3)
          universe3
        }
        val pf = ParticleFilter(static, universe2, trans(_, _), numParticles)
        pf.start()
        pf.advanceTime(List(NamedEvidence("y", Observation(true))))
        val qxTrue = 0.2 * 0.8
        val qxFalse = 0.8 * 0.1
        val pxTrue = qxTrue / (qxTrue + qxFalse)
        pf.currentProbability("x", true) should be(pxTrue +- 0.01)
      }
    }

    "iterating over two time steps with evidence on each step" should {
      "contain a state with the correct probability" in {
        val numParticles = 100000
        val universe1 = createNew()
        Flip(0.2)("f1", universe1)
        def trans(previous: Universe): Universe = {
          val universe2 = createNew()
          val previousF1 = previous.get("f1").asInstanceOf[Element[Boolean]]
          val f1 = If(previousF1, Flip(0.8), Flip(0.3))("f1", universe2)
          val f2 = If(f1, Flip(0.6), Flip(0.1))("f2", universe2)
          universe2
        }
        val pf = ParticleFilter(universe1, trans, numParticles)
        pf.start()
        pf.advanceTime(List(NamedEvidence("f2", Observation(true))))
        val result1 = pf.currentProbability("f1", true)
        pf.advanceTime(List(NamedEvidence("f2", Observation(false))))
        val result2 = pf.currentProbability("f1", true)
        val pF11T = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
        val pF11F = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
        val answer1 = pF11T / (pF11T + pF11F)
        val pF10T_F11T_F21T_F12T_F22F = 0.2 * 0.8 * 0.6 * 0.8 * 0.4
        val pF10T_F11T_F21T_F12F_F22F = 0.2 * 0.8 * 0.6 * 0.2 * 0.9
        val pF10T_F11F_F21T_F12T_F22F = 0.2 * 0.2 * 0.1 * 0.3 * 0.4
        val pF10T_F11F_F21T_F12F_F22F = 0.2 * 0.2 * 0.1 * 0.7 * 0.9
        val pF10F_F11T_F21T_F12T_F22F = 0.8 * 0.3 * 0.6 * 0.8 * 0.4
        val pF10F_F11T_F21T_F12F_F22F = 0.8 * 0.3 * 0.6 * 0.2 * 0.9
        val pF10F_F11F_F21T_F12T_F22F = 0.8 * 0.7 * 0.1 * 0.3 * 0.4
        val pF10F_F11F_F21T_F12F_F22F = 0.8 * 0.7 * 0.1 * 0.7 * 0.9
        val pF12T = pF10T_F11T_F21T_F12T_F22F + pF10T_F11F_F21T_F12T_F22F + pF10F_F11T_F21T_F12T_F22F + pF10F_F11F_F21T_F12T_F22F
        val pF12F = pF10T_F11T_F21T_F12F_F22F + pF10T_F11F_F21T_F12F_F22F + pF10F_F11T_F21T_F12F_F22F + pF10F_F11F_F21T_F12F_F22F
        result1 should be(answer1 +- 0.01)
        val answer2 = pF12T / (pF12T + pF12F)
        result2 should be(answer2 +- 0.01)
      }

      "correctly estimate static variables" in {
        val numParticles = 100000
        val static = createNew()
        val x = Flip(0.2)("x", static)
        val initial = createNew()
        val y = Flip(0.3)("y", initial)
        def trans(static: Universe, previous: Universe): Universe = {
          val universe3 = createNew()
          val y = If(static.get[Boolean]("x"), Flip(0.8), previous.get[Boolean]("y"))("y", universe3)
          universe3
        }
        val pf = ParticleFilter(static, initial, trans(_, _), numParticles)
        pf.start()
        pf.advanceTime(List(NamedEvidence("y", Observation(true))))
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
        pf.currentProbability("x", true) should be(pxTrue +- 0.01)
      }
    }

    "iterating over two time steps with evidence on each step" should {
      "contain a the correct distribution over an element" in {
        val numParticles = 100000
        val universe1 = createNew()
        Flip(0.2)("f1", universe1)
        def trans(previous: Universe): Universe = {
          val universe2 = createNew()
          val previousF1 = previous.get("f1").asInstanceOf[Element[Boolean]]
          val f1 = If(previousF1, Flip(0.8), Flip(0.3))("f1", universe2)
          val f2 = If(f1, Flip(0.6), Flip(0.1))("f2", universe2)
          universe2
        }
        val pf = ParticleFilter(universe1, trans, numParticles)
        pf.start()
        pf.advanceTime(List(NamedEvidence("f2", Observation(true))))
        pf.advanceTime(List(NamedEvidence("f2", Observation(false))))
        val qf1TrueTime1 = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
        val qf1FalseTime1 = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
        val pf1TrueTime1 = qf1TrueTime1 / (qf1TrueTime1 + qf1FalseTime1)
        val pf1FalseTime1 = 1 - pf1TrueTime1
        val qf1TrueTime2 = (pf1TrueTime1 * 0.8 + pf1FalseTime1 * 0.3) * 0.4
        val qf1FalseTime2 = (pf1TrueTime1 * 0.2 + pf1FalseTime1 * 0.7) * 0.9
        val pf1TrueTime2 = qf1TrueTime2 / (qf1TrueTime2 + qf1FalseTime2)
        val d = pf.currentDistribution("f1").asInstanceOf[Stream[(Double, Boolean)]]
        d.size should equal(2)
        if (d(0)._2 == true) {
          d(0)._2 should equal(true)
          d(1)._2 should equal(false)
          d(0)._1 should be(pf1TrueTime2 +- 0.01)
          d(1)._1 should be((1.0 - pf1TrueTime2) +- 0.01)
        } else {
          d(1)._2 should equal(true)
          d(0)._2 should equal(false)
          d(1)._1 should be(pf1TrueTime2 +- 0.01)
          d(0)._1 should be((1.0 - pf1TrueTime2) +- 0.01)
        }
      }
    }

    "iterating over many time steps" should {
      "not suffer from memory leaks" taggedAs (Performance) in {
        val numParticles = 1000
        val numSteps = 1000
        val universe1 = createNew()
        Constant(Array.fill(1000)(0))("f1", universe1)
        def trans(previous: Universe): Universe = {
          val universe2 = createNew()
          val previousF1 = previous.get[Array[Int]]("f1")
          Apply(previousF1, (a: Array[Int]) => a.map(_ + 1))("f1", universe2)
          universe2
        }
        val pf = ParticleFilter(universe1, trans, numParticles)
        pf.start()
        for { i <- 1 to numSteps } {
          pf.advanceTime(List())
        }
      }
    }
  }
}
