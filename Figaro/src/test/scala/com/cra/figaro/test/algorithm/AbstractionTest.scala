/*
 * AbstractionTest.scala 
 * Abstraction tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import com.cra.figaro.test.tags.NonDeterministic

class AbstractionTest extends WordSpec with Matchers {
  "A regular discretization" should {
    "select to an ordered range with the specified number of elements in which the bins " +
      "are of equal size" in {
        Universe.createNew()
        val inputs = List(0.2, 0.6, 0.3, 0.4, 0.8)
        val points = AbstractionScheme.regularDiscretization.select(inputs, 3).toList
        points.length should equal(3)
        points(0) should be(0.3 +- 0.00000001)
        points(1) should be(0.5 +- 0.00000001)
        points(2) should be(0.7 +- 0.00000001)
      }
  }

  "A Double element when adding abstraction pragmas without providing an explicit abstraction scheme" should {
    "have a pragma after adding it" in {
      Universe.createNew()
      val u = Uniform(0.0, 1.0)
      val a = Abstraction[Double](10)
      u.addPragma(a)
      assert(u.pragmas contains a)
    }

    "have an added pragma's abstraction scheme be discretization" in {
      Universe.createNew()
      val u = Uniform(0.0, 1.0)
      val a = Abstraction[Double](10)
      u.addPragma(a)
      assert(a.scheme == AbstractionScheme.regularDiscretization)
    }

    "have fromPragmas return the most recently added abstraction" in {
      Universe.createNew()
      val u = Uniform(0.0, 1.0)
      val a1 = Abstraction[Double](10)
      val a2 = Abstraction[Double](5)
      u.addPragma(a1)
      u.addPragma(a2)
      u.addPragma(new Pragma[Double] {})
      Abstraction.fromPragmas(u.pragmas) should equal(Some(a2))
    }

    "have fromPragmas return None if no abstractions have been added" in {
      Universe.createNew()
      val u = Uniform(0.0, 1.0)
      u.addPragma(new Pragma[Double] {})
      Abstraction.fromPragmas(u.pragmas) should equal(None)
    }
  }

  "Computing the values of an abstract element" when {
    "given an atomic element using a regular discretization scheme" should {
      "produce an sequence of the correct size of uniformly distributed values" taggedAs(NonDeterministic) in {
        Universe.createNew()
        val u = Uniform(0.0, 1.0)
        u.addPragma(Abstraction(100)(AbstractionScheme.RegularDiscretization))
        val v = Values()(u).toList.sorted
        v.length should equal(100)

        v.sorted should equal(v)
        v(0) should be(0.005 +- 0.005)
        v(99) should be(0.995 +- 0.005)
        val diff = v(1) - v(0)
        for { i <- 2 to 99 } { v(i) - v(i - 1) should be(diff +- 0.0000001) }
      }
    }

    "given a chain whose results are atomic elements using a regular discretization scheme" should {
      "produce a sequence ranging from the minimum to maximum of all the results" in {
        Universe.createNew()
        def f(d: Double) = {
          val u = Uniform(d, d + 1)
          u.addPragma(Abstraction(100)(AbstractionScheme.RegularDiscretization))
          u
        }
        val c = Chain(Select(0.5 -> 0.0, 0.5 -> 3.0), f)
        c.addPragma(Abstraction(20)(AbstractionScheme.RegularDiscretization))

        val v = Values()(c).toList.sorted
        v.head should be(0.1 +- 0.02)
        v.last should be(3.9 +- 0.02)
      }
    }

    "given an Apply2 whose arguments are atomic elements using a regular discretization scheme" should {
      "produce a sequence ranging from the minimum to maximum of all the results" in {
        Universe.createNew()
        val u = Uniform(0.0, 2.0)
        u.addPragma(Abstraction(200)(AbstractionScheme.RegularDiscretization))
        val a = Apply(u, u, ((d1: Double, d2: Double) => d1 * d2))
        a.addPragma(Abstraction(20)(AbstractionScheme.RegularDiscretization))

        val v = Values()(a).toList.sorted
        v.head should be(0.1 +- 0.05)
        v.last should be(3.9 +- 0.05)
      }
    }
  }

  "Creating a list of factors for an abstract element" when {
    "given an atomic argument using a regular discretization scheme" should {
      "produce a single factor whose rows correspond to the discrete values with their associated densities" in {
        Universe.createNew()
        val max = 2.0
        val numBins = 20
        val uniform = Uniform(0.0, max)
        uniform.addPragma(Abstraction(numBins)(AbstractionScheme.RegularDiscretization))
        Values()(uniform)
        val factors = Factory.make(uniform)
        factors.size should equal(1)
        val factor = factors(0)
        val variable = Variable(uniform)
        factor.variables should equal(List(variable))
        val allIndices = factor.allIndices
        allIndices.size should equal(numBins)
        for { indices <- allIndices } {
          factor.get(indices) should be(1.0 / max +- 0.000001) // constant density of Uniform(0, max)
        }
      }
    }

    "given an apply of one argument using a regular discretization scheme" should {
      "produce a single factor mapping the argument to the discretized result" in {
        Universe.createNew()
        val max = 2.0
        val numBinsUniform = 20
        val numBinsApply = 10
        val uniform = Uniform(0.0, max)
        uniform.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val apply = Apply(uniform, (d: Double) => d * d)
        apply.addPragma(Abstraction(numBinsApply)(AbstractionScheme.RegularDiscretization))
        Values()(apply)
        val factors = Factory.make(apply)
        factors.size should equal(1)
        val factor = factors(0)
        val uniformVariable = Variable(uniform)
        val applyVariable = Variable(apply)
        factor.variables should equal(List(uniformVariable, applyVariable))
        factor.allIndices.size should equal(numBinsUniform * numBinsApply)
        val uniformValues: List[Double] = uniformVariable.range.map(_.value)
        val applyValues: List[Double] = applyVariable.range.map(_.value)
        def check(uniformValue: Double, applyValue: Double): Boolean = {
          val resultValue = uniformValue * uniformValue
          def minDiff: Double =
            (Double.MaxValue /: applyValues)((d1: Double, d2: Double) => d1 min math.abs(resultValue - d2))
          math.abs(resultValue - applyValue) <= minDiff
        }
        for {
          i <- 0 until numBinsUniform
          j <- 0 until numBinsApply
        } {
          if (check(uniformValues(i), applyValues(j))) { factor.get(List(i, j)) should equal(1.0) }
          else { factor.get(List(i, j)) should equal(0.0) }
        }
      }
    }

    "given an apply of two arguments using a regular discretization scheme" should {
      "produce a single factor mapping the arguments to the discretized result" in {
        Universe.createNew()
        val max = 2.0
        val numBinsUniform = 5
        val numBinsApply = 10
        val uniform1 = Uniform(0.0, max)
        uniform1.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val uniform2 = Uniform(0.0, max)
        uniform2.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val apply = Apply(uniform1, uniform2, (d1: Double, d2: Double) => d1 * d2)
        apply.addPragma(Abstraction(numBinsApply)(AbstractionScheme.RegularDiscretization))
        Values()(apply)
        val factors = Factory.make(apply)
        factors.size should equal(1)
        val factor = factors(0)
        val uniform1Variable = Variable(uniform1)
        val uniform2Variable = Variable(uniform2)
        val applyVariable = Variable(apply)
        factor.variables should equal(List(uniform1Variable, uniform2Variable, applyVariable))
        factor.allIndices.size should equal(numBinsUniform * numBinsUniform * numBinsApply)
        val uniform1Values: List[Double] = uniform1Variable.range.map(_.value)
        val uniform2Values: List[Double] = uniform2Variable.range.map(_.value)
        val applyValues: List[Double] = applyVariable.range.map(_.value)
        def check(uniform1Value: Double, uniform2Value: Double, applyValue: Double): Boolean = {
          val resultValue = uniform1Value * uniform2Value
          def minDiff: Double =
            (Double.MaxValue /: applyValues)((d1: Double, d2: Double) => d1 min math.abs(resultValue - d2))
          math.abs(resultValue - applyValue) <= minDiff
        }
        for {
          i <- 0 until numBinsUniform
          j <- 0 until numBinsUniform
          k <- 0 until numBinsApply
        } {
          if (check(uniform1Values(i), uniform2Values(j), applyValues(k))) {
            factor.get(List(i, j, k)) should equal(1.0)
          } else { factor.get(List(i, j, k)) should equal(0.0) }
        }
      }
    }

    "given an apply of three arguments using a regular discretization scheme" should {
      "produce a single factor mapping the arguments to the discretized result" in {
        Universe.createNew()
        val max = 2.0
        val numBinsUniform = 5
        val numBinsApply = 10
        val uniform1 = Uniform(0.0, max)
        uniform1.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val uniform2 = Uniform(0.0, max)
        uniform2.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val uniform3 = Uniform(0.0, max)
        uniform3.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val apply = Apply(uniform1, uniform2, uniform3, (d1: Double, d2: Double, d3: Double) => d1 * d2 * d3)
        apply.addPragma(Abstraction(numBinsApply)(AbstractionScheme.RegularDiscretization))
        Values()(apply)
        val factors = Factory.make(apply)
        factors.size should equal(1)
        val factor = factors(0)
        val uniform1Variable = Variable(uniform1)
        val uniform2Variable = Variable(uniform2)
        val uniform3Variable = Variable(uniform3)
        val applyVariable = Variable(apply)
        factor.variables should equal(List(uniform1Variable, uniform2Variable, uniform3Variable, applyVariable))
        factor.allIndices.size should equal(numBinsUniform * numBinsUniform * numBinsUniform * numBinsApply)
        val uniform1Values: List[Double] = uniform1Variable.range.map(_.value)
        val uniform2Values: List[Double] = uniform2Variable.range.map(_.value)
        val uniform3Values: List[Double] = uniform3Variable.range.map(_.value)
        val applyValues: List[Double] = applyVariable.range.map(_.value)
        def check(uniform1Value: Double, uniform2Value: Double, uniform3Value: Double, applyValue: Double): Boolean = {
          val resultValue = uniform1Value * uniform2Value * uniform3Value
          def minDiff: Double =
            (Double.MaxValue /: applyValues)((d1: Double, d2: Double) => d1 min math.abs(resultValue - d2))
          math.abs(resultValue - applyValue) <= minDiff
        }
        for {
          i <- 0 until numBinsUniform
          j <- 0 until numBinsUniform
          k <- 0 until numBinsUniform
          l <- 0 until numBinsApply
        } {
          if (check(uniform1Values(i), uniform2Values(j), uniform3Values(k), applyValues(l))) {
            factor.get(List(i, j, k, l)) should equal(1.0)
          } else { factor.get(List(i, j, k, l)) should equal(0.0) }
        }
      }
    }

    "given a chain using a regular discretization scheme" should {
      "produce a list of conditional selector factors mapping the argument to the discretized result" in {
        Universe.createNew()
        val numBinsUniform = 3
        val numBinsChain = 4
        val flip = Flip(0.5)
        val uniform1 = Uniform(0.0, 1.0)
        val uniform2 = Uniform(1.0, 2.0)
        uniform1.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        uniform2.addPragma(Abstraction(numBinsUniform)(AbstractionScheme.RegularDiscretization))
        val chain = Chain(flip, (b: Boolean) => if (b) uniform1; else uniform2)
        chain.addPragma(Abstraction(numBinsChain)(AbstractionScheme.RegularDiscretization))
        Values()(chain)
        val factors = Factory.make(chain)
        factors.size should equal(2) // 2 for the conditional selectors
        val List(factor1, factor2) = factors
        val flipVariable = Variable(flip)
        val uniform1Variable = Variable(uniform1)
        val uniform2Variable = Variable(uniform2)
        val chainVariable = Variable(chain)
        factor1.variables should equal(List(flipVariable, chainVariable, uniform1Variable))
        factor1.allIndices.size should equal(2 * numBinsChain * numBinsUniform)
        factor2.allIndices.size should equal(2 * numBinsChain * numBinsUniform)
        val flipValues: List[Boolean] = flipVariable.range.map(_.value)
        val uniform1Values: List[Double] = uniform1Variable.range.map(_.value)
        val uniform2Values: List[Double] = uniform2Variable.range.map(_.value)
        val chainValues: List[Double] = chainVariable.range.map(_.value)
        def closest(chainValue: Double, uniformValue: Double): Boolean = {
          def minDiff: Double =
            (Double.MaxValue /: chainValues)((d1: Double, d2: Double) => d1 min math.abs(uniformValue - d2))
          math.abs(uniformValue - chainValue) <= minDiff
        }
        def check1(flipValue: Boolean, chainValue: Double, uniformValue: Double): Boolean =
          !flipValue || closest(chainValue, uniformValue)
        def check2(flipValue: Boolean, chainValue: Double, uniformValue: Double): Boolean =
          flipValue || closest(chainValue, uniformValue)
        for {
          i <- 0 to 1
          j <- 0 until numBinsChain
          k <- 0 until numBinsUniform
        } {
          if (check1(flipValues(i), chainValues(j), uniform1Values(k))) { factor1.get(List(i, j, k)) should equal(1.0) }
          else { factor1.get(List(i, j, k)) should equal(0.0) }
        }
        for {
          i <- 0 to 1
          j <- 0 until numBinsChain
          k <- 0 until numBinsUniform
        } {
          if (check2(flipValues(i), chainValues(j), uniform2Values(k))) { factor2.get(List(i, j, k)) should equal(1.0) }
          else { factor2.get(List(i, j, k)) should equal(0.0) }
        }
      }
    }
  }

  "Running variable elimination on a model with multiple discretized elements" should {
    "produce the correct answer" taggedAs(NonDeterministic) in {
      Universe.createNew()
      val flip = Flip(0.5)
      val uniform1 = Uniform(0.0, 1.0)
      val uniform2 = Uniform(1.0, 2.0)
      val chain = If(flip, uniform1, uniform2)
      val apply = Apply(chain, (d: Double) => d + 1.0)
      apply.addConstraint((d: Double) => d)
      uniform1.addPragma(Abstraction(10))
      uniform2.addPragma(Abstraction(10))
      chain.addPragma(Abstraction(10))
      apply.addPragma(Abstraction(10))
      val ve = VariableElimination(flip)
      ve.start()
      /*
       * Probability computation:
       * Undiscretized:
       * Uniform1 will result in (1,2) with expected weight 1.5.
       * Uniform2 will result in (2,3) with expected weight 2.5.
       * Therefore flip should be around 0.4 for true.
       */
      ve.probability(flip, (b: Boolean) => b) should be(0.4 +- 0.02)
      ve.kill
    }
  }
}

