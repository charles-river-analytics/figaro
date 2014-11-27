package com.cra.figaro.test.library.process

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.language._
import com.cra.figaro.library.process._
import com.cra.figaro.algorithm.sampling.{Importance, MetropolisHastings, ProposalScheme}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.library.atomic.discrete.Geometric
import com.cra.figaro.algorithm.lazyfactored.LazyValues

class VariableSizeArrayTest extends WordSpec with Matchers {

  "A variable size array" should {
    "correctly create random lists of the appropriate lengths" in {
      Universe.createNew()
      def generator(n: Int) = Flip(1.0 / (n + 1))
      val lengthChooser = Select(0.4 -> 2, 0.6 -> 3)
      val array = Container(lengthChooser, generator _)
      val all = array.forall((b: Boolean) => b)
      val p1 = 1.0 / 1 * 1.0 / 2
      val p2 = p1 * 1.0 / 3
      val answer = 0.4 * p1 + 0.6 * p2
      Importance.probability(all, true) should be (answer +- 0.02)
    }

    "always use the same elements for the value at an index" in {
      Universe.createNew()
      var count = 0
      def generator(n: Int) = { count += 1; Flip(1.0 / (n + 1)) }
      val lengthChooser = Select(0.5 -> 1, 0.5 -> 2)
      val array = Container(lengthChooser, generator _)
      lengthChooser.observe(1) // causes the array element to generate
      lengthChooser.observe(2) // causes the array element to generate
      count should equal (2) // proves that we've only generated two elements, so the first one was shared
    }

    "have the number of items be distributed according to the first argument" in {
      Universe.createNew()
      val x1 = Geometric(0.3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val x3 = x2.length
      val answer = 0.3 * 0.3 * 0.7
      Importance.probability(x3, 3) should be(answer +- 0.01)
    }

    "have each item be distributed according to the element generator" in {
      Universe.createNew()
      val x1 = Select(0.5 -> 2, 0.5 -> 3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val x3 = x2(0)
      val x4 = x2(1)
      val answer = 0.2
      VariableElimination.probability(x3, true) should be(answer +- 0.0000001)
      VariableElimination.probability(x4, true) should be(answer +- 0.0000001)
    }

    "have the items be generated independently" in {
      Universe.createNew()
      val x1 = Select(0.5 -> 2, 0.5 -> 3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val x3 = x2(0)
      val x4 = x2(1)
      val x5 = x3 === x4
      val answer = 0.2 * 0.2 + 0.8 * 0.8
      VariableElimination.probability(x5, true) should be(answer +- 0.0000001)
    }

    "have the correct set of possible values, generating values for the items" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val values = LazyValues(Universe.universe)
      val valueSet = values(x2.element, Integer.MAX_VALUE)
      valueSet.hasStar should equal (false)
      val regVals = valueSet.regularValues.toList
      regVals.size should equal (2)
      val container0 = regVals(0).asInstanceOf[Container[Int, Boolean]]
      val container1 = regVals(1).asInstanceOf[Container[Int, Boolean]]
      val item00 = container0(0)
      val item01 = container0(1)
      val item10 = container1(0)
      val item11 = container1(1)
      val item12 = container1(2)
      for { item <- List(item00, item01, item10, item11, item12) } {
        val itemValueSet = values.storedValues(item)
        itemValueSet.hasStar should equal (false)
        itemValueSet.regularValues should equal (Set(false, true))
      }
    }

    "return the correct probability under importance sampling" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val x3 = x2.exists { (b: Boolean) => b }
      x3.observe(true)
      val alg = Importance(10000, x1)
      alg.start()
      val p2 = 0.4 * (1 - 0.8 * 0.8)
      val p3 = 0.6 * (1 - 0.8 * 0.8 * 0.8)
      val answer = p3 / (p2 + p3)
      alg.probability(x1, 3) should be(answer +- 0.01)
      alg.kill()
    }

    "return the correct probability under variable elimination" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val x3 = x2.exists { (b: Boolean) => b }
      x3.observe(true)
      val alg = VariableElimination(x1)
      alg.start()
      val p2 = 0.4 * (1 - 0.8 * 0.8)
      val p3 = 0.6 * (1 - 0.8 * 0.8 * 0.8)
      val answer = p3 / (p2 + p3)
      alg.probability(x1, 3) should be(answer +- 0.0000001)
      alg.kill()
    }

    "return the correct probability under Metropolis-Hastings" in {
      Universe.createNew()
      val x1 = Select(0.4 -> 2, 0.6 -> 3)
      val x2 = Container(x1, (i: Int) => Flip(0.2))
      val x3 = x2.exists { (b: Boolean) => b }
      x3.observe(true)
      val alg = MetropolisHastings(200000, ProposalScheme.default, x1)
      alg.start()
      val p2 = 0.4 * (1 - 0.8 * 0.8)
      val p3 = 0.6 * (1 - 0.8 * 0.8 * 0.8)
      val answer = p3 / (p2 + p3)
      alg.probability(x1, 3) should be(answer +- 0.02)
      alg.kill()
    }
  }
}
