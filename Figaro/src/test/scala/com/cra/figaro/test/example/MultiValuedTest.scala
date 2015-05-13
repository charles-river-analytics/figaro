/*
 * MultiValuedTest.scala
 * Multi-valued reference uncertainty example tests.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.util._

class MultiValuedTest extends WordSpec with Matchers {
  "A model with multi-valued reference uncertainty" should {
    "produce the correct distribution over values under importance sampling" in {
      Universe.createNew()

      val specialComponent1 = new Component { override val name = "Special 1" }

      def makeComponent() = Select(0.2 -> specialComponent1, 0.8 -> new Component)

      class Container extends ElementCollection {
        val components = MakeList(Select(0.3 -> 1, 0.7 -> 2), makeComponent)("components", this)
        val sum = getAggregate((xs: MultiSet[Int]) => (0 /: xs)(_ + _))("components.f")
      }

      val c = new Container
      specialComponent1.f.observe(2)
      val alg = Importance(20000, c.sum)
      alg.start()
      // Possible lists:
      val pSpecial = 0.3 * 0.2
      val pNew = 0.3 * 0.8
      val pSpecialSpecial = 0.7 * 0.2 * 0.2
      val pSpecialNew = 0.7 * 0.2 * 0.8
      val pNewSpecial = 0.7 * 0.8 * 0.2
      val pNewNew = 0.7 * 0.8 * 0.8
      val p2 = pSpecial * 1.0 + pNew * 0.4 + pSpecialSpecial * 1.0 // by set semantics
      val p3 = pSpecial * 0.0 + pNew * 0.6 + pSpecialSpecial * 0.0
      val p4 = pSpecialNew * 0.4 + pNewSpecial * 0.4 + pNewNew * 0.4 * 0.4
      val p5 = pSpecialNew * 0.6 + pNewSpecial * 0.6 + pNewNew * (0.4 * 0.6 + 0.6 * 0.4)
      val p6 = pSpecialNew * 0.0 + pNewSpecial * 0.0 + pNewNew * 0.6 * 0.6
      alg.probability(c.sum, 2) should be(p2 +- 0.01)
      alg.probability(c.sum, 3) should be(p3 +- 0.01)
      alg.probability(c.sum, 4) should be(p4 +- 0.01)
      alg.probability(c.sum, 5) should be(p5 +- 0.01)
      alg.probability(c.sum, 6) should be(p6 +- 0.01)
      alg.kill
    }

    "produce the correct distribution over values under variable elimination" in {
      Universe.createNew()

      val specialComponent1 = new Component { override val name = "Special 1" }

      def makeComponent() = Select(0.2 -> specialComponent1, 0.8 -> new Component)

      class Container extends ElementCollection {
        val components = MakeList(Select(0.3 -> 1, 0.7 -> 2), makeComponent)("components", this)
        val sum = getAggregate((xs: MultiSet[Int]) => (0 /: xs)(_ + _))("components.f")
      }

      val c = new Container
      specialComponent1.f.observe(2)
      val alg = VariableElimination(c.sum)
      alg.start()
      // Possible lists:
      val pSpecial = 0.3 * 0.2
      val pNew = 0.3 * 0.8
      val pSpecialSpecial = 0.7 * 0.2 * 0.2
      val pSpecialNew = 0.7 * 0.2 * 0.8
      val pNewSpecial = 0.7 * 0.8 * 0.2
      val pNewNew = 0.7 * 0.8 * 0.8
      val p2 = pSpecial * 1.0 + pNew * 0.4 + pSpecialSpecial * 1.0 // by set semantics
      val p3 = pSpecial * 0.0 + pNew * 0.6 + pSpecialSpecial * 0.0
      val p4 = pSpecialNew * 0.4 + pNewSpecial * 0.4 + pNewNew * 0.4 * 0.4
      val p5 = pSpecialNew * 0.6 + pNewSpecial * 0.6 + pNewNew * (0.4 * 0.6 + 0.6 * 0.4)
      val p6 = pSpecialNew * 0.0 + pNewSpecial * 0.0 + pNewNew * 0.6 * 0.6
      alg.probability(c.sum, 2) should be(p2 +- 0.0000001)
      alg.probability(c.sum, 3) should be(p3 +- 0.0000001)
      alg.probability(c.sum, 4) should be(p4 +- 0.0000001)
      alg.probability(c.sum, 5) should be(p5 +- 0.0000001)
      alg.probability(c.sum, 6) should be(p6 +- 0.0000001)
      alg.kill
    }

    "produce the correct distribution over values under Metropolis-Hastings" in {
      Universe.createNew()

      val specialComponent1 = new Component { override val name = "Special 1" }

      def makeComponent() = Select(0.2 -> specialComponent1, 0.8 -> new Component)

      class Container extends ElementCollection {
        val components = MakeList(Select(0.3 -> 1, 0.7 -> 2), makeComponent)("components", this)
        val sum = getAggregate((xs: MultiSet[Int]) => (0 /: xs)(_ + _))("components.f")
      }

      val c = new Container
      specialComponent1.f.observe(2)
      val alg = MetropolisHastings(100000, ProposalScheme.default, c.sum)
      alg.start()
      // Possible lists:
      val pSpecial = 0.3 * 0.2
      val pNew = 0.3 * 0.8
      val pSpecialSpecial = 0.7 * 0.2 * 0.2
      val pSpecialNew = 0.7 * 0.2 * 0.8
      val pNewSpecial = 0.7 * 0.8 * 0.2
      val pNewNew = 0.7 * 0.8 * 0.8
      val p2 = pSpecial * 1.0 + pNew * 0.4 + pSpecialSpecial * 1.0 // by set semantics
      val p3 = pSpecial * 0.0 + pNew * 0.6 + pSpecialSpecial * 0.0
      val p4 = pSpecialNew * 0.4 + pNewSpecial * 0.4 + pNewNew * 0.4 * 0.4
      val p5 = pSpecialNew * 0.6 + pNewSpecial * 0.6 + pNewNew * (0.4 * 0.6 + 0.6 * 0.4)
      val p6 = pSpecialNew * 0.0 + pNewSpecial * 0.0 + pNewNew * 0.6 * 0.6
      alg.probability(c.sum, 2) should be(p2 +- 0.01)
      alg.probability(c.sum, 3) should be(p3 +- 0.01)
      alg.probability(c.sum, 4) should be(p4 +- 0.01)
      alg.probability(c.sum, 5) should be(p5 +- 0.01)
      alg.probability(c.sum, 6) should be(p6 +- 0.01)
      alg.kill
    }
  }

  class Component extends ElementCollection {
    Component.id += 1
    val name = "Component " + Component.id
    val f = Select(0.4 -> 2, 0.6 -> 3)("f", this)
    override def toString = name
  }

  object Component {
    var id: Int = 0
  }

}
