/*
 * ParParticleFilterTest.scala 
 * Parallel particle filter tests.
 * 
 * Created By:      Lee Kellogg (lkellogg@cra.com)
 * Creation Date:   Jun 2, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
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

class ParParticleFilterTest extends WordSpec with PrivateMethodTester with Matchers {
  
  val numThreads = 8

  "A parallel particle filter" when {
    "constructing a belief state from a set of weighted samples" should {
      "contain a sample with fraction proportional to its weight" in {
        val numParticles = 10000
        val genWithStates = () => {
          val staticSnapshot = new Snapshot
          val universe = new Universe()
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
          (universe, state1, state2)
        }
        val gen = () => genWithStates()._1
        val pf = ParticleFilter.par(() => gen(), (u: Universe) => new Universe(), numParticles, numThreads)
        val pfClass = pf.getClass.getSuperclass
        val (_, state1, state2) = genWithStates()
        val weightedParticles: Seq[ParticleFilter.WeightedParticle] = List((0.4, state1), (0.6, state2))

        pf.updateBeliefState(weightedParticles)
        (pf.beliefState count (_ == state1)).toDouble / numParticles should be(0.4 +- 0.01)
      }
    }

    "constructing the initial belief state" should {
      "contain a state with fraction proportional to its probability" taggedAs(NonDeterministic) in {
        val numParticles = 20000
        val gen = () => {
          val universe = new Universe()
          val f1 = Flip(0.2)("f1", universe)
          val i2 = If(f1, Flip(0.3)(Name.default, universe), Flip(0.6)(Name.default, universe))("f2", universe)
          i2.observe(true)
          universe
        }
        val qf1True = 0.2 * 0.3
        val qf1False = 0.8 * 0.6
        val pf1True = qf1True / (qf1True + qf1False)
        val pf = ParticleFilter.par(gen, (u: Universe) => new Universe(), numParticles, numThreads)
        pf.start()
        pf.currentProbability("f1", true) should be(pf1True +- 0.01)
      }
    }

    "constructing a new belief state with no evidence" should {
      "contain a state with fraction proportional to its expected probability under the initial belief state" in {
        try {
        val numParticles = 20000
        val gen = () => {
          val universe1 = new Universe()
          val f1 = Flip(0.2)("f", universe1)
          universe1
        }
        def trans(u: Universe): Universe = {
          val universe2 = new Universe()
          val f2 = If(u.get[Boolean]("f"), Flip(0.8)(Name.default, universe2), Flip(0.3)(Name.default, universe2))("f", universe2)
          universe2
        }
        val pf = ParticleFilter.par(gen, trans, numParticles, numThreads)
        pf.start()
        pf.advanceTime(List())
        val p = 0.2 * 0.8 + 0.8 * 0.3
        pf.currentProbability("f", true) should be(p +- 0.01)
        } catch {
          case e: java.util.NoSuchElementException => e.printStackTrace()
        }
      }
    }

    "constructing a new belief state with evidence" should {
      "contain a state with fraction proportional to its expected probability under the initial belief state" +
        "conditioned on the evidence" in {
          val numParticles = 50000
          val gen = () => {
            val universe1 = new Universe()
            val f11 = Flip(0.2)("f1", universe1)
            universe1
          }
          def trans(u: Universe): Universe = {
            val universe2 = new Universe()
            val f12 = If(u.get[Boolean]("f1"), Flip(0.8)(Name.default, universe2), Flip(0.3)(Name.default, universe2))("f1", universe2)
            val f22 = If(f12, Flip(0.6)(Name.default, universe2), Flip(0.1)(Name.default, universe2))("f2", universe2)
            universe2
          }
          val pf = ParticleFilter.par(gen, trans, numParticles, numThreads)
          pf.start()
          pf.advanceTime(List(NamedEvidence("f2", Observation(true))))
          val qf1True = (0.2 * 0.8 + 0.8 * 0.3) * 0.6
          val qf1False = (0.2 * 0.2 + 0.8 * 0.7) * 0.1
          val pf1True = qf1True / (qf1True + qf1False)
          pf.currentProbability("f1", true) should be(pf1True +- 0.01)
        }

      "correctly estimate static variables" in {
        val numParticles = 100000
        val staticGen = () => {
          val static = new Universe()
          val x = Flip(0.2)("x", static)
          static
        }
        val gen = () => {
          val universe2 = new Universe()
          universe2
        }
        def trans(static: Universe, previous: Universe): Universe = {
          val universe3 = new Universe()
          val y = If(static.get[Boolean]("x"), Flip(0.8)(Name.default, universe3), Flip(0.1)(Name.default, universe3))("y", universe3)
          universe3
        }
        val pf = ParticleFilter.par(staticGen, gen, trans(_, _), numParticles, numThreads)
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
        val gen = () => {
          val universe1 = new Universe()
          Flip(0.2)("f1", universe1)
          universe1
        }
        def trans(previous: Universe): Universe = {
          val universe2 = new Universe()
          val previousF1 = previous.get("f1").asInstanceOf[Element[Boolean]]
          val f1 = If(previousF1, Flip(0.8)(Name.default, universe2), Flip(0.3)(Name.default, universe2))("f1", universe2)
          val f2 = If(f1, Flip(0.6)(Name.default, universe2), Flip(0.1)(Name.default, universe2))("f2", universe2)
          universe2
        }
        val pf = ParticleFilter.par(gen, trans, numParticles, numThreads)
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
        val staticGen = () => {
          val static = new Universe()
          val x = Flip(0.2)("x", static)
          static
        }
        val initialGen = () => {
          val initial = new Universe()
          val y = Flip(0.3)("y", initial)
          initial
        }
        def trans(static: Universe, previous: Universe): Universe = {
          val universe3 = new Universe()
          val y = If(static.get[Boolean]("x"), Flip(0.8)(Name.default, universe3), previous.get[Boolean]("y"))("y", universe3)
          universe3
        }
        val pf = ParticleFilter.par(staticGen, initialGen, trans(_, _), numParticles, numThreads)
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
        val gen = () => {
          val universe1 = new Universe()
          Flip(0.2)("f1", universe1)
          universe1
        }
        def trans(previous: Universe): Universe = {
          val universe2 = new Universe()
          val previousF1 = previous.get("f1").asInstanceOf[Element[Boolean]]
          val f1 = If(previousF1, Flip(0.8)(Name.default, universe2), Flip(0.3)(Name.default, universe2))("f1", universe2)
          val f2 = If(f1, Flip(0.6)(Name.default, universe2), Flip(0.1)(Name.default, universe2))("f2", universe2)
          universe2
        }
        val pf = ParticleFilter.par(gen, trans, numParticles, numThreads)
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
        val gen = () => {
          val universe1 = new Universe()
          Constant(Array.fill(1000)(0))("f1", universe1)
          universe1
        }
        def trans(previous: Universe): Universe = {
          val universe2 = new Universe()
          val previousF1 = previous.get[Array[Int]]("f1")
          Apply(previousF1, (a: Array[Int]) => a.map(_ + 1))("f1", universe2)
          universe2
        }
        val pf = ParticleFilter.par(gen, trans, numParticles, numThreads)
        pf.start()
        for { i <- 1 to numSteps } {
          pf.advanceTime(List())
        }
      }
    }
    
    "splitting up the particle indices over threads" should {
      
      val numParticles = 1000
      val numThreads = 7
      val pf = ParticleFilter.par(() => new Universe(), (u) => new Universe(), numParticles, numThreads)
      val calculateIndices = PrivateMethod[Seq[(Int, Int)]]('calculateIndices)
      val indices = pf invokePrivate calculateIndices(numParticles, numThreads)
      
      "assign work to each thread" in {
        indices.length should equal(numThreads)
      }
      
      val lengths = indices map { case (start, end) => end - start + 1 }
      
      "assign all the indices" in {
        lengths.sum should equal(numParticles)
      }
      
      "assign the indices evenly" in {
        (lengths.max - lengths.min) should (equal(0) or equal(1))
      }
    }
    
    "creating and running algorithm over multiple threads" should {
      
      val numParticles = 1000
      val numThreads = 8
      
      var staticCalledCount = 0
      val static = () => {
        staticCalledCount += 1
        new Universe()
      }
      
      var initCalledCount = 0
      val initial = () => {
        initCalledCount += 1
        new Universe()
      }
      
      var transCalledCount = 0
      val transition = (u1: Universe, u2: Universe) => {
        transCalledCount += 1
        new Universe()
      }
      val pf = ParticleFilter.par(static, initial, transition, numParticles, numThreads)
      
      "start with an empty belief state" in {
        pf.beliefState.toList.count(_ == null) should equal(numParticles)
      }
      
      "initialize universes for each thread when algorithm starts" in {
        staticCalledCount should equal(0)
        initCalledCount should equal(0)
        transCalledCount should equal(0)
        pf.run()
        staticCalledCount should equal(numThreads)
        initCalledCount should equal(numThreads)
        transCalledCount should equal(0)
      }
      
      "call transition function for each thread every time step" in {
        val numSteps = 1000
        for (_ <- 1 to numSteps) {
          pf.advanceTime()
        }
        staticCalledCount should equal(numThreads)
        initCalledCount should equal(numThreads)
        transCalledCount should equal(numSteps * numThreads)
      }
      
      "update belief state with particles from all threads" in {
        pf.beliefState.length should equal(numParticles)
        pf.beliefState.toList.count(_ == null) should equal(0)
      }
    }
  }
}
