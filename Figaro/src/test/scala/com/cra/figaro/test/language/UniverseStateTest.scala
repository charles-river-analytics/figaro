/*
 * UniverseStateTest.scala
 * Tests for saving and restoring universe state.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 3, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.language

import com.cra.figaro.algorithm.OneTime
import com.cra.figaro.language.Element.ElemVal
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{Normal, Uniform}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable

class UniverseStateTest extends WordSpec with Matchers {
  "A universe state" when {
    "restoring elements in the universe" should {
      "restore value and randomness" in {
        val universe = Universe.createNew()
        val e1 = Uniform(0.0, 1.0)
        e1.generate()
        val e1Randomness = e1.randomness
        val e1Value = e1.value
        val state = new UniverseState(universe)

        e1.generate()

        state.restore()
        e1.randomness should equal(e1Randomness)
        e1.value should equal(e1Value)
      }

      "restore set and unset elements" in {
        val universe = Universe.createNew()
        val e1 = Normal(0.0, 1.0)
        val e2 = Uniform(0.0, 1.0)
        e1.set(100.0)
        e2.generate()
        val state = new UniverseState(universe)

        e1.unset()
        e1.generate()
        e2.set(0.5)

        state.restore()
        e1.generate() // Won't produce anything other than 100.0 unless e1 is unset
        e2.generate() // Produces something other than 0.5 with probability 1
        e1.value should equal(100.0)
        e2.value should not equal 0.5
      }

      "restore previous evidence" when {
        /*
         * Test for:
         *   Conditions, constraints, observations
         *   Contingent and non-contingent evidence
         *   Adding and removing evidence
         */

        "conditions are changed" in {
          val universe = Universe.createNew()
          val e1 = Uniform(0.0, 1.0)
          val e2 = Flip(0.2)
          e1.addCondition(_ > 0.3)
          e1.addCondition(_ < 0.8, List(ElemVal(e2, true)))
          val e1Conditions = e1.allConditions
          val state = new UniverseState(universe)

          e1.removeConditions() // Only removes the first (non-contingent) condition
          e1.addCondition(_ > 0.5, List(ElemVal(e2, false)))
          e2.addCondition((b: Boolean) => b)

          state.restore()
          e1.allConditions should contain theSameElementsAs e1Conditions
          e2.allConditions shouldBe empty
          universe.conditionedElements should contain theSameElementsAs List(e1)
        }

        "constraints are changed" in {
          val universe = Universe.createNew()
          val e1 = Uniform(0.0, 1.0)
          val e2 = Flip(0.2)
          e1.addConstraint(_ * 2.0)
          e1.addConstraint(_ + 1.0, List(ElemVal(e2, true)))
          val e1Constraints = e1.allConstraints
          val state = new UniverseState(universe)

          e1.removeConstraints() // Only removes the first (non-contingent) constraint
          e1.addConstraint(_ * 3.0, List(ElemVal(e2, false)))
          e2.addConstraint((b: Boolean) => if(b) 2.0 else 1.0)

          state.restore()
          e1.allConstraints should contain theSameElementsAs e1Constraints
          e2.allConstraints shouldBe empty
          universe.constrainedElements should contain theSameElementsAs List(e1)
        }

        "observations are changed" in {
          val universe = Universe.createNew()
          val e1 = Uniform(0.0, 1.0)
          val e2 = Flip(0.2)
          e1.observe(0.3)
          val state = new UniverseState(universe)

          e1.unobserve()
          e2.observe(true)

          state.restore()
          e1.observation should equal(Some(0.3))
          e2.observation should equal(None)
          universe.conditionedElements should contain theSameElementsAs List(e1)
        }
      }

      "activate the correct elements" when {
        "elements are deactivated manually" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          val e2 = Constant(7)
          e2.deactivate()
          val state = new UniverseState(universe)

          e1.deactivate()
          e2.activate()

          state.restore()
          e1.active should be(true)
          e2.active should be(false)
          universe.activeElements should contain theSameElementsAs List(e1)
        }

        "temporary elements are cleared" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          universe.pushContext(e1)
          val e2 = Constant(7) // This makes e2 temporary
          val state = new UniverseState(universe)

          universe.clearTemporaries()

          state.restore()
          universe.activeElements should contain theSameElementsAs List(e1, e2)
        }

        "the universe is cleared" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          val state = new UniverseState(universe)

          universe.clear()

          state.restore()
          e1.active should be(true)
          universe.activeElements should contain theSameElementsAs List(e1)
        }
      }

      "restore context and direct context contents" in {
        val universe = Universe.createNew()
        val e1 = Constant(8)
        universe.pushContext(e1)
        val e2 = Constant(7)
        val state = new UniverseState(universe)

        e2.deactivate()

        state.restore()
        e1.directContextContents should contain theSameElementsAs List(e2)
        e2.context should contain theSameElementsAs List(e1)
      }
    }

    "restoring information about the universe structure" should {
      "copy the previous context stack" when {
        "elements in the stack are deactivated" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          universe.pushContext(e1)
          val e2 = Constant(7)
          universe.pushContext(e2)
          val e3 = Constant(6)
          universe.pushContext(e3)
          val state = new UniverseState(universe)

          e2.deactivate()

          state.restore()
          universe.contextStack should equal(List(e3, e2, e1))
        }

        "the context stack is modified manually" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          universe.pushContext(e1)
          val e2 = Constant(7)
          universe.pushContext(e2)
          val e3 = Constant(6)
          universe.pushContext(e3)
          val state = new UniverseState(universe)

          universe.popContext(e2)

          state.restore()
          universe.contextStack should equal(List(e3, e2, e1))
        }

        "temporary elements are cleared" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          universe.pushContext(e1)
          val e2 = Constant(7)
          universe.pushContext(e2)
          val e3 = Constant(6)
          universe.pushContext(e3)
          val state = new UniverseState(universe)

          universe.clearTemporaries()

          state.restore()
          universe.contextStack should equal(List(e3, e2, e1))
        }

        "the universe is cleared" in {
          val universe = Universe.createNew()
          val e1 = Constant(8)
          universe.pushContext(e1)
          val state = new UniverseState(universe)

          universe.clear()

          state.restore()
          universe.contextStack should equal(List(e1))
        }
      }

      "repopulate the same stochastic elements" in {
        val universe = Universe.createNew()
        val e1 = Normal(0.0, 1.0)
        val e2 = Uniform(0.0, 1.0)
        e2.deactivate()
        val state = new UniverseState(universe)

        e1.deactivate()
        e2.activate()

        state.restore()
        universe.stochasticElements should contain theSameElementsAs List(e1)
      }

      "repopulate uses and usedBy" when {
        "elements in the maps are deactivated" in {

        }

        "new uses are registered" in {

        }
      }

      "not modify registered maps" when {
        "registered algorithms change" in {
          val universe = Universe.createNew()
          var alg1Killed = false
          var alg2Killed = false
          val alg1 = new OneTime { override def run() = {} ; override def kill() = { alg1Killed = true } }
          val alg2 = new OneTime { override def run() = {} ; override def kill() = { alg2Killed = true }  }
          alg1.start()
          alg2.start()
          universe.registerAlgorithm(alg1)
          universe.deregisterAlgorithm(alg2)
          val state = new UniverseState(universe)

          universe.deregisterAlgorithm(alg1)
          universe.registerAlgorithm(alg2)

          state.restore()
          universe.clear() // Kills registered algorithms (i.e. alg2 because state shouldn't modify them)
          alg1Killed should be(false)
          alg2Killed should be(true)
        }

        "registered element maps change" in {

        }

        "registered universe maps change" in {
          val universe = Universe.createNew()
          val map1 = mutable.Map(universe -> 1)
          val map2 = mutable.Map(universe -> 2)
          universe.registerUniverse(map1)
          val state = new UniverseState(universe)

          universe.deregisterUniverse(map1)
          universe.registerUniverse(map2)

          state.restore()
          universe.clear() // Removes universe from registered maps (i.e. map2 because state shouldn't modify them)
          map1 should contain key universe
          map2 should not contain key(universe)
        }
      }
    }
  }
}
