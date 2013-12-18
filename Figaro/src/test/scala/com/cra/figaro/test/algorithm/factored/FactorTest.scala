/*
 * FactorTest.scala 
 * Factor tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._

class FactorTest extends WordSpec with ShouldMatchers with PrivateMethodTester {

  "A variable for an element" should {
    "have range equal to the element's values" in {
      Universe.createNew()
      val e1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val v1 = Variable(e1)
      for { x <- v1.range } { Values()(e1) should contain(x) }
      for { x <- Values()(e1) } { v1.range should contain(x) }
    }

    "throw UnsupportedAlgorithmException if the range cannot be computed" in {
      Universe.createNew()
      val e1 = Uniform(0.0, 1.0)
      evaluating { Variable(e1) } should produce[UnsupportedAlgorithmException]
    }

    "always be equal to another variable for the same element" in {
      Universe.createNew()
      val e1 = Flip(0.2)
      val v1 = Variable(e1)
      val v2 = Variable(e1)
      v1 should equal(v2)
    }

    "be different to a variable for a different element with the same definition" in {
      Universe.createNew()
      val v1 = Variable(Flip(0.2))
      val v2 = Variable(Flip(0.2))
      v1 should not equal (v2)
    }
  }

  "A factor" when {
    // Only need to test private methods and variables if public tests do not work
    "get the same value for a given set of variable indices as was last set" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = new Factor[Double](List(v1, v2, v3, v4))
      val indices = List(1, 0, 2, 1)
      f.set(indices, 0.2)
      f.set(indices, 0.3)
      f.get(indices) should equal(0.3)
    }

    "have the first index List be all zeros" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = new Factor[Double](List(v1, v2, v3, v4))
      f.firstIndices should equal(List(0, 0, 0, 0))
    }

    "have the next index List carry and add correctly" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = new Factor[Double](List(v1, v2, v3, v4))
      val ia = List(1, 0, 1, 1)
      val ar = f.nextIndices(ia).get
      ar should equal(List(0, 0, 2, 1))
    }

    "produce None when the index Lists are exhausted" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = new Factor[Double](List(v1, v2, v3, v4))
      val ia = List(1, 0, 2, 1)
      f.nextIndices(ia) should equal(None)
    }

    "compute the union of variables in two factors and the correct index maps when calling unionVars" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      val e5 = Constant('a)
      val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val v5 = Variable(e5)
      val v6 = Variable(e6)
      val f = new Factor[Double](List(v1, v2, v3, v4))
      val g = new Factor[Double](List(v5, v3, v2, v6))
      val unionVars = PrivateMethod[(List[Variable[_]], List[Int], List[Int])]('unionVars)
      val (union, indexMap1, indexMap2) = f invokePrivate unionVars(g)
      union should equal(List(v1, v2, v3, v4, v5, v6))
      indexMap1 should equal(List(0, 1, 2, 3))
      indexMap2 should equal(List(4, 2, 1, 5))
    }

    "multiplying with another factor" should {
      "return the product of the two factors" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        val e4 = Flip(0.6)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val f = new Factor[Double](List(v1, v2, v3))
        val g = new Factor[Double](List(v4, v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        g.set(List(0, 0), 0.6)
        g.set(List(1, 0), 0.7)
        g.set(List(0, 1), 0.8)
        g.set(List(1, 1), 0.9)
        val h = f.product(g, (x: Double, y: Double) => x * y)
        h.variables should equal(List(v1, v2, v3, v4))
        h.get(List(0, 0, 0, 0)) should be(0.0 plusOrMinus 0.0001)
        h.get(List(1, 0, 0, 0)) should be(0.06 plusOrMinus 0.0001)
        h.get(List(2, 0, 0, 0)) should be(0.12 plusOrMinus 0.0001)
        h.get(List(0, 0, 1, 0)) should be(0.24 plusOrMinus 0.0001)
        h.get(List(1, 0, 1, 0)) should be(0.32 plusOrMinus 0.0001)
        h.get(List(2, 0, 1, 0)) should be(0.4 plusOrMinus 0.0001)
        h.get(List(0, 0, 0, 1)) should be(0.0 plusOrMinus 0.0001)
        h.get(List(1, 0, 0, 1)) should be(0.07 plusOrMinus 0.0001)
        h.get(List(2, 0, 0, 1)) should be(0.14 plusOrMinus 0.0001)
        h.get(List(0, 0, 1, 1)) should be(0.27 plusOrMinus 0.0001)
        h.get(List(1, 0, 1, 1)) should be(0.36 plusOrMinus 0.0001)
        h.get(List(2, 0, 1, 1)) should be(0.45 plusOrMinus 0.0001)
      }
    }

    "calling sumOver on a variable" should {
      "return the sum over the variable of the factor" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = new Factor[Double](List(v1, v2, v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.sumOver(v3, (x: Double, y: Double) => x + y, 0.0)
        g.variables should equal(List(v1, v2))
        g.get(List(0, 0)) should be(0.3 plusOrMinus 0.0001)
        g.get(List(1, 0)) should be(0.5 plusOrMinus 0.0001)
        g.get(List(2, 0)) should be(0.7 plusOrMinus 0.0001)
      }

      "return itself if the variable not in the factor" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = new Factor[Double](List(v1, v2))
        f.set(List(0, 0), 0.0)
        f.set(List(1, 0), 0.2)
        f.set(List(2, 0), 0.4)
        val g = f.sumOver(v3, (x: Double, y: Double) => x + y, 0.0)
        g.variables should equal(f.variables)
        for { indices <- f.allIndices } {
          g.get(indices) should equal(f.get(indices))
        }
      }

      "return a factor with all columns of the variable removed, ignoring rows in which " +
        "the variable has different values in different columns" in {
          Universe.createNew()
          val e1 = Flip(0.9)
          val e2 = Select(0.2 -> 1, 0.8 -> 2)
          val v1 = Variable(e1)
          val v2 = Variable(e2)
          val f = new Factor[Double](List(v1, v2, v1))
          f.set(List(0, 0, 0), 0.1)
          f.set(List(1, 0, 0), 0.2)
          f.set(List(0, 1, 0), 0.3)
          f.set(List(1, 1, 0), 0.4)
          f.set(List(0, 0, 1), 0.5)
          f.set(List(1, 0, 1), 0.6)
          f.set(List(0, 1, 1), 0.7)
          f.set(List(1, 1, 1), 0.8)
          val g = f.sumOver(v1, (x: Double, y: Double) => x + y, 0.0)
          g.variables should equal(List(v2))
          g.get(List(0)) should equal(0.1 + 0.6)
          g.get(List(1)) should equal(0.3 + 0.8)
        }
    }

    "calling record on a variable" should {
      "return the argmax over the values associated with the variable" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = new Factor[Double](List(v1, v2, v3))
        f.set(List(0, 0, 0), 0.6)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.recordArgMax(v3, (x: Double, y: Double) => x < y)
        g.variables should equal(List(v1, v2))
        g.get(List(0, 0)) should equal(true)
        g.get(List(1, 0)) should equal(false)
        g.get(List(2, 0)) should equal(false)
      }
    }

    "after marginalizing to a variable" should {
      "return the marginal distribution over the variable" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = new Factor[Double](List(v1, v2, v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.marginalizeTo(v3, (x: Double, y: Double) => x + y, 0.0)
        g.variables should equal(List(v3))
        val p1 = 0.0 + 0.1 + 0.2
        val p2 = 0.3 + 0.4 + 0.5
        g.get(List(0)) should be(p1 plusOrMinus 0.000001)
        g.get(List(1)) should be(p2 plusOrMinus 0.000001)
      }
    }
  }

  "Making factors from an element" when {
    "given a continuous element (such as Uniform)" should {
      "raise UnsupportedAlgorithmException" in {
        Universe.createNew()
        val v1 = Uniform(0.2, 1.0)
        evaluating { ProbFactor.make(v1) } should produce[UnsupportedAlgorithmException]
      }
    }

    "given a constant" should {
      "produce a single factor with one entry whose value is 1.0" in {
        Universe.createNew()
        val v1 = Constant(7)
        val List(factor) = ProbFactor.make(v1)
        factor.get(List(0)) should equal(1.0)
      }
    }

    "given a simple flip" should {
      "produce a single factor in which the first entry is the probability of true " +
        "and the second entry is the probability of false" in {
          Universe.createNew()
          val v1 = Flip(0.3)
          val List(factor) = ProbFactor.make(v1)
          factor.get(List(0)) should equal(0.3)
          factor.get(List(1)) should equal(0.7)
        }
    }

    "given a complex flip" should {
      "produce a single factor in which each possible value of the parent is associated with two " +
        "entries, one each for true and false, with the appropriate probabilities" in {
          Universe.createNew()
          val v1 = Select(0.2 -> 0.1, 0.8 -> 0.3)
          val v2 = Flip(v1)
          val List(factor) = ProbFactor.make(v2)
          val i1 = Values(Universe.universe)(v1).toList.indexOf(0.1)
          val i2 = Values(Universe.universe)(v1).toList.indexOf(0.3)
          factor.get(List(i1, 0)) should equal(0.1)
          factor.get(List(i1, 1)) should equal(0.9)
          factor.get(List(i2, 0)) should equal(0.3)
          factor.get(List(i2, 1)) should equal(0.7)
        }
    }

    "given a simple select" should {
      "produce a single factor in which each possible value is associated with the correct probability" in {
        Universe.createNew()
        val v1 = Select(0.2 -> 1, 0.3 -> 0, 0.1 -> 2, 0.05 -> 5, 0.35 -> 4)
        val List(factor) = ProbFactor.make(v1)
        val vals = Values(Universe.universe)(v1).toList
        val i1 = vals.indexOf(1)
        val i0 = vals.indexOf(0)
        val i2 = vals.indexOf(2)
        val i5 = vals.indexOf(5)
        val i4 = vals.indexOf(4)
        factor.get(List(i1)) should equal(0.2)
        factor.get(List(i0)) should equal(0.3)
        factor.get(List(i2)) should equal(0.1)
      }
    }

    "given a complex select" should {
      "produce a single factor in which values of the parents are associated with " +
        "values of the select according to the normalized parent values" in {
          Universe.createNew()
          val v1 = Select(0.2 -> 0.2, 0.8 -> 0.8)
          val v2 = Select(0.4 -> 0.4, 0.6 -> 0.6)
          val c1 = Constant(0.1)
          val c2 = Constant(0.3)
          val c3 = Constant(0.5)
          val v3 = Select(v1 -> 1, v2 -> 2, c1 -> 4, c2 -> 5, c3 -> 3)
          val List(factor) = ProbFactor.make(v3)
          val v1Vals = Values()(v1).toList
          val v2Vals = Values()(v2).toList
          val v3Vals = Values()(v3).toList
          val v102 = v1Vals.indexOf(0.2)
          val v108 = v1Vals.indexOf(0.8)
          val v204 = v2Vals.indexOf(0.4)
          val v206 = v2Vals.indexOf(0.6)
          val v31 = v3Vals.indexOf(1)
          val v32 = v3Vals.indexOf(2)
          val v34 = v3Vals.indexOf(4)
          val v35 = v3Vals.indexOf(5)
          val v33 = v3Vals.indexOf(3)
          def makeIndices(a: List[Int]): List[Int] = {
            val result: Array[Int] = Array.ofDim(a.size)
            result(0) = a(0)
            result(v31 + 1) = a(1)
            result(v32 + 1) = a(2)
            result(v33 + 1) = a(3)
            result(v34 + 1) = a(4)
            result(v35 + 1) = a(5)
            result.toList
          }
          factor.get(makeIndices(List(v31, v102, v204, 0, 0, 0))) should be(0.2 / 1.5 plusOrMinus 0.01)
          factor.get(makeIndices(List(v32, v102, v204, 0, 0, 0))) should be(0.4 / 1.5 plusOrMinus 0.01)
          factor.get(makeIndices(List(v31, v108, v204, 0, 0, 0))) should be(0.8 / 2.1 plusOrMinus 0.01)
          factor.get(makeIndices(List(v32, v108, v204, 0, 0, 0))) should be(0.4 / 2.1 plusOrMinus 0.01)
          factor.get(makeIndices(List(v31, v102, v206, 0, 0, 0))) should be(0.2 / 1.7 plusOrMinus 0.01)
          factor.get(makeIndices(List(v32, v102, v206, 0, 0, 0))) should be(0.6 / 1.7 plusOrMinus 0.01)
          factor.get(makeIndices(List(v31, v108, v206, 0, 0, 0))) should be(0.8 / 2.3 plusOrMinus 0.01)
          factor.get(makeIndices(List(v32, v108, v206, 0, 0, 0))) should be(0.6 / 2.3 plusOrMinus 0.01)
        }
    }

    "given a simple dist" should {
      "produce a list of factors, one for each outcome and one representing the choice over outcomes; " +
        "the factor for an outcome matches the outcome value to the dist value" in {
          Universe.createNew()
          val v1 = Flip(0.2)
          val v2 = Constant(false)
          val v3 = Dist(0.3 -> v1, 0.7 -> v2)
          val v1Vals = Values()(v1).toList
          val v3Vals = Values()(v3).toList
          val v1TrueIndex = v1Vals.indexOf(true)
          val v1FalseIndex = v1Vals.indexOf(false)
          val v3TrueIndex = v3Vals.indexOf(true)
          val v3FalseIndex = v3Vals.indexOf(false)
          val v1Index = v3.outcomes.indexOf(v1)
          val v2Index = v3.outcomes.indexOf(v2)
          val selectFactor :: outcomeFactors = ProbFactor.make(v3)
          outcomeFactors.size should equal(2)
          val v1Factor = outcomeFactors(v1Index)
          val v2Factor = outcomeFactors(v2Index)
          selectFactor.get(List(v1Index)) should equal(0.3)
          selectFactor.get(List(v2Index)) should equal(0.7)
          v1Factor.get(List(v1Index, v3TrueIndex, v1TrueIndex)) should equal(1.0)
          v1Factor.get(List(v1Index, v3TrueIndex, v1FalseIndex)) should equal(0.0)
          v1Factor.get(List(v1Index, v3FalseIndex, v1TrueIndex)) should equal(0.0)
          v1Factor.get(List(v1Index, v3FalseIndex, v1FalseIndex)) should equal(1.0)
          for { i <- 0 to 1; j <- 0 to 1 } v1Factor.get(List(v2Index, i, j)) should equal(1.0)
          v2Factor.get(List(v2Index, v3FalseIndex, 0)) should equal(1.0)
          v2Factor.get(List(v2Index, v3TrueIndex, 0)) should equal(0.0)
          for { i <- 0 to 1; j <- 0 to 0 } v2Factor.get(List(v1Index, i, j)) should equal(1.0)
        }
    }

    "given a complex dist" should {
      "produce a list of factors, one for each outcome and one representing the choice over outcomes; " +
        "the factor for an outcome matches the outcome value to the dist value" in {
          Universe.createNew()
          val v1 = Select(0.2 -> 0.2, 0.8 -> 0.8)
          val v2 = Select(0.4 -> 0.4, 0.6 -> 0.6)
          val v3 = Flip(0.2)
          val v4 = Constant(false)
          val v5 = Dist(v1 -> v3, v2 -> v4)
          val v1Vals = Values()(v1).toList
          val v2Vals = Values()(v2).toList
          val v3Vals = Values()(v3).toList
          val v4Vals = Values()(v4).toList
          val v5Vals = Values()(v5).toList
          val v3Index = v5.outcomes.indexOf(v3)
          val v4Index = v5.outcomes.indexOf(v4)
          val v102 = v1Vals.indexOf(0.2)
          val v108 = v1Vals.indexOf(0.8)
          val v204 = v2Vals.indexOf(0.4)
          val v206 = v2Vals.indexOf(0.6)
          val v3f = v3Vals.indexOf(false)
          val v3t = v3Vals.indexOf(true)
          val v5f = v5Vals.indexOf(false)
          val v5t = v5Vals.indexOf(true)
          val selectFactor :: outcomeFactors = ProbFactor.make(v5)
          outcomeFactors.size should equal(2)
          val v1Factor = outcomeFactors(v3Index)
          val v2Factor = outcomeFactors(v4Index)
          selectFactor.get(List(0, v102, v204)) should be(0.2 / 0.6 plusOrMinus 0.0001)
          selectFactor.get(List(1, v102, v204)) should be(0.4 / 0.6 plusOrMinus 0.0001)
          selectFactor.get(List(0, v102, v206)) should be(0.2 / 0.8 plusOrMinus 0.0001)
          selectFactor.get(List(1, v102, v206)) should be(0.6 / 0.8 plusOrMinus 0.0001)
          selectFactor.get(List(0, v108, v204)) should be(0.8 / 1.2 plusOrMinus 0.0001)
          selectFactor.get(List(1, v108, v204)) should be(0.4 / 1.2 plusOrMinus 0.0001)
          selectFactor.get(List(0, v108, v206)) should be(0.8 / 1.4 plusOrMinus 0.0001)
          selectFactor.get(List(1, v108, v206)) should be(0.6 / 1.4 plusOrMinus 0.0001)
          v1Factor.get(List(0, v5t, v3t)) should equal(1.0)
          v1Factor.get(List(0, v5t, v3f)) should equal(0.0)
          v1Factor.get(List(0, v5f, v3t)) should equal(0.0)
          v1Factor.get(List(0, v5f, v3f)) should equal(1.0)
          for { i <- 0 to 1; j <- 0 to 1 } v1Factor.get(List(1, i, j)) should equal(1.0)
          v2Factor.get(List(1, v5f, 0)) should equal(1.0)
          v2Factor.get(List(1, v5t, 0)) should equal(0.0)
          for { i <- 0 to 1; j <- 0 to 0 } v2Factor.get(List(0, i, j)) should equal(1.0)
        }
    }

    "given a chain" should {
      "produce a conditional selector for each parent value" in {
        Universe.createNew()
        val v1 = Flip(0.2)
        val v2 = Select(0.1 -> 1, 0.9 -> 2)
        val v3 = Constant(3)
        val v4 = Chain(v1, (b: Boolean) => if (b) v2; else v3)
        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v4Vals = Values()(v4).toList
        val v1t = v1Vals indexOf true
        val v1f = v1Vals indexOf false
        val v21 = v2Vals indexOf 1
        val v22 = v2Vals indexOf 2
        val v41 = v4Vals indexOf 1
        val v42 = v4Vals indexOf 2
        val v43 = v4Vals indexOf 3
        Expand()
        val List(v2Factor, v3Factor) = ProbFactor.make(v4)
        v2Factor.get(List(v1t, v41, v21)) should equal(1.0)
        v2Factor.get(List(v1t, v41, v22)) should equal(0.0)
        v2Factor.get(List(v1t, v42, v21)) should equal(0.0)
        v2Factor.get(List(v1t, v42, v22)) should equal(1.0)
        v2Factor.get(List(v1t, v43, v21)) should equal(0.0)
        v2Factor.get(List(v1t, v43, v22)) should equal(0.0)
        for { i <- 0 to 1; j <- 0 to 1 } v2Factor.get(List(v1f, i, j)) should equal(1.0)
        v3Factor.get(List(v1f, v41, 0)) should equal(0.0)
        v3Factor.get(List(v1f, v42, 0)) should equal(0.0)
        v3Factor.get(List(v1f, v43, 0)) should equal(1.0)
        for { i <- 0 to 1; j <- 0 to 0 } v3Factor.get(List(v1t, i, j)) should equal(1.0)
      }
    }

    "given an apply of one argument" should {
      "produce a factor that matches the argument to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Apply(v1, (i: Int) => i % 2)
        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        val v20 = v2Vals indexOf 0
        val v21 = v2Vals indexOf 1
        val List(factor) = ProbFactor.make(v2)
        factor.get(List(v11, v20)) should equal(0.0)
        factor.get(List(v11, v21)) should equal(1.0)
        factor.get(List(v12, v20)) should equal(1.0)
        factor.get(List(v12, v21)) should equal(0.0)
        factor.get(List(v13, v20)) should equal(0.0)
        factor.get(List(v13, v21)) should equal(1.0)
      }
    }

    "given an apply of two arguments" should {
      "produce a factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 2, 0.5 -> 3)
        val v3 = Apply(v1, v2, (i: Int, j: Int) => i % j)
        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v3Vals = Values()(v3).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        val v22 = v2Vals indexOf 2
        val v23 = v2Vals indexOf 3
        val v30 = v3Vals indexOf 0
        val v31 = v3Vals indexOf 1
        val v32 = v3Vals indexOf 2
        val List(factor) = ProbFactor.make(v3)
        factor.get(List(v11, v22, v30)) should equal(0.0)
        factor.get(List(v11, v22, v31)) should equal(1.0)
        factor.get(List(v11, v22, v32)) should equal(0.0)
        factor.get(List(v11, v23, v30)) should equal(0.0)
        factor.get(List(v11, v23, v31)) should equal(1.0)
        factor.get(List(v11, v23, v32)) should equal(0.0)
        factor.get(List(v12, v22, v30)) should equal(1.0)
        factor.get(List(v12, v22, v31)) should equal(0.0)
        factor.get(List(v12, v22, v32)) should equal(0.0)
        factor.get(List(v12, v23, v30)) should equal(0.0)
        factor.get(List(v12, v23, v31)) should equal(0.0)
        factor.get(List(v12, v23, v32)) should equal(1.0)
        factor.get(List(v13, v22, v30)) should equal(0.0)
        factor.get(List(v13, v22, v31)) should equal(1.0)
        factor.get(List(v13, v22, v32)) should equal(0.0)
        factor.get(List(v13, v23, v30)) should equal(1.0)
        factor.get(List(v13, v23, v31)) should equal(0.0)
        factor.get(List(v13, v23, v32)) should equal(0.0)
      }
    }

    "given an apply of three arguments" should {
      "produce a factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4: Apply3[Int, Int, Int, Int] = Apply(v1, v2, v3, (i: Int, j: Int, k: Int) => i % (j + k))
        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v3Vals = Values()(v3).toList
        val v4Vals = Values()(v4).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        val v21 = v2Vals indexOf 1
        val v22 = v2Vals indexOf 2
        val v31 = v3Vals indexOf 1
        val v40 = v4Vals indexOf 0
        val v41 = v4Vals indexOf 1
        val v42 = v4Vals indexOf 2
        val List(factor) = ProbFactor.make(v4)
        factor.get(List(v11, v21, v31, v40)) should equal(0.0)
        factor.get(List(v11, v21, v31, v41)) should equal(1.0)
        factor.get(List(v11, v21, v31, v42)) should equal(0.0)
        factor.get(List(v11, v22, v31, v40)) should equal(0.0)
        factor.get(List(v11, v22, v31, v41)) should equal(1.0)
        factor.get(List(v11, v22, v31, v42)) should equal(0.0)
        factor.get(List(v12, v21, v31, v40)) should equal(1.0)
        factor.get(List(v12, v21, v31, v41)) should equal(0.0)
        factor.get(List(v12, v21, v31, v42)) should equal(0.0)
        factor.get(List(v12, v22, v31, v40)) should equal(0.0)
        factor.get(List(v12, v22, v31, v41)) should equal(0.0)
        factor.get(List(v12, v22, v31, v42)) should equal(1.0)
        factor.get(List(v13, v21, v31, v40)) should equal(0.0)
        factor.get(List(v13, v21, v31, v41)) should equal(1.0)
        factor.get(List(v13, v21, v31, v42)) should equal(0.0)
        factor.get(List(v13, v22, v31, v40)) should equal(1.0)
        factor.get(List(v13, v22, v31, v41)) should equal(0.0)
        factor.get(List(v13, v22, v31, v42)) should equal(0.0)
      }
    }

    "given an apply of four arguments" should {
      "produce a factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4 = Flip(0.7)
        val v5: Apply4[Int, Int, Int, Boolean, Int] =
          Apply(v1, v2, v3, v4, (i: Int, j: Int, k: Int, b: Boolean) => if (b) 0; else i % (j + k))
        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v3Vals = Values()(v3).toList
        val v4Vals = Values()(v4).toList
        val v5Vals = Values()(v5).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        val v21 = v2Vals indexOf 1
        val v22 = v2Vals indexOf 2
        val v31 = v3Vals indexOf 1
        val v4true = v4Vals indexOf true
        val v4false = v4Vals indexOf false
        val v50 = v5Vals indexOf 0
        val v51 = v5Vals indexOf 1
        val v52 = v5Vals indexOf 2
        val List(factor) = ProbFactor.make(v5)
        factor.get(List(v11, v21, v31, v4false, v50)) should equal(0.0)
        factor.get(List(v11, v21, v31, v4false, v51)) should equal(1.0)
        factor.get(List(v11, v21, v31, v4false, v52)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4false, v50)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4false, v51)) should equal(1.0)
        factor.get(List(v11, v22, v31, v4false, v52)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4false, v50)) should equal(1.0)
        factor.get(List(v12, v21, v31, v4false, v51)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4false, v52)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4false, v50)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4false, v51)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4false, v52)) should equal(1.0)
        factor.get(List(v13, v21, v31, v4false, v50)) should equal(0.0)
        factor.get(List(v13, v21, v31, v4false, v51)) should equal(1.0)
        factor.get(List(v13, v21, v31, v4false, v52)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4false, v50)) should equal(1.0)
        factor.get(List(v13, v22, v31, v4false, v51)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4false, v52)) should equal(0.0)

        factor.get(List(v11, v21, v31, v4true, v50)) should equal(1.0)
        factor.get(List(v11, v21, v31, v4true, v51)) should equal(0.0)
        factor.get(List(v11, v21, v31, v4true, v52)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4true, v50)) should equal(1.0)
        factor.get(List(v11, v22, v31, v4true, v51)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4true, v52)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4true, v50)) should equal(1.0)
        factor.get(List(v12, v21, v31, v4true, v51)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4true, v52)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4true, v50)) should equal(1.0)
        factor.get(List(v12, v22, v31, v4true, v51)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4true, v52)) should equal(0.0)
        factor.get(List(v13, v21, v31, v4true, v50)) should equal(1.0)
        factor.get(List(v13, v21, v31, v4true, v51)) should equal(0.0)
        factor.get(List(v13, v21, v31, v4true, v52)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4true, v50)) should equal(1.0)
        factor.get(List(v13, v22, v31, v4true, v51)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4true, v52)) should equal(0.0)
      }
    }

    "given an apply of five arguments" should {
      "produce a factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4 = Flip(0.7)
        val v5 = Constant(false)
        val v6: Apply5[Int, Int, Int, Boolean, Boolean, Int] =
          Apply(v1, v2, v3, v4, v5,
            (i: Int, j: Int, k: Int, b: Boolean, c: Boolean) => if (b || c) 0; else i % (j + k))
        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v3Vals = Values()(v3).toList
        val v4Vals = Values()(v4).toList
        val v5Vals = Values()(v5).toList
        val v6Vals = Values()(v6).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        val v21 = v2Vals indexOf 1
        val v22 = v2Vals indexOf 2
        val v31 = v3Vals indexOf 1
        val v4true = v4Vals indexOf true
        val v4false = v4Vals indexOf false
        val v5false = v5Vals indexOf false
        val v60 = v6Vals indexOf 0
        val v61 = v6Vals indexOf 1
        val v62 = v6Vals indexOf 2
        val List(factor) = ProbFactor.make(v6)

        factor.get(List(v11, v21, v31, v4false, v5false, v60)) should equal(0.0)
        factor.get(List(v11, v21, v31, v4false, v5false, v61)) should equal(1.0)
        factor.get(List(v11, v21, v31, v4false, v5false, v62)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4false, v5false, v60)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4false, v5false, v61)) should equal(1.0)
        factor.get(List(v11, v22, v31, v4false, v5false, v62)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4false, v5false, v60)) should equal(1.0)
        factor.get(List(v12, v21, v31, v4false, v5false, v61)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4false, v5false, v62)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4false, v5false, v60)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4false, v5false, v61)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4false, v5false, v62)) should equal(1.0)
        factor.get(List(v13, v21, v31, v4false, v5false, v60)) should equal(0.0)
        factor.get(List(v13, v21, v31, v4false, v5false, v61)) should equal(1.0)
        factor.get(List(v13, v21, v31, v4false, v5false, v62)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4false, v5false, v60)) should equal(1.0)
        factor.get(List(v13, v22, v31, v4false, v5false, v61)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4false, v5false, v62)) should equal(0.0)

        factor.get(List(v11, v21, v31, v4true, v5false, v60)) should equal(1.0)
        factor.get(List(v11, v21, v31, v4true, v5false, v61)) should equal(0.0)
        factor.get(List(v11, v21, v31, v4true, v5false, v62)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4true, v5false, v60)) should equal(1.0)
        factor.get(List(v11, v22, v31, v4true, v5false, v61)) should equal(0.0)
        factor.get(List(v11, v22, v31, v4true, v5false, v62)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4true, v5false, v60)) should equal(1.0)
        factor.get(List(v12, v21, v31, v4true, v5false, v61)) should equal(0.0)
        factor.get(List(v12, v21, v31, v4true, v5false, v62)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4true, v5false, v60)) should equal(1.0)
        factor.get(List(v12, v22, v31, v4true, v5false, v61)) should equal(0.0)
        factor.get(List(v12, v22, v31, v4true, v5false, v62)) should equal(0.0)
        factor.get(List(v13, v21, v31, v4true, v5false, v60)) should equal(1.0)
        factor.get(List(v13, v21, v31, v4true, v5false, v61)) should equal(0.0)
        factor.get(List(v13, v21, v31, v4true, v5false, v62)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4true, v5false, v60)) should equal(1.0)
        factor.get(List(v13, v22, v31, v4true, v5false, v61)) should equal(0.0)
        factor.get(List(v13, v22, v31, v4true, v5false, v62)) should equal(0.0)
      }
    }

    "given an Inject" should {
      "produces a factor that matches its inputs to the correct sequence" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 4, 0.5 -> 5)
        val v3 = Inject(v1, v2)
        val List(factor) = ProbFactor.make(v3)

        val v1Vals = Values()(v1).toList
        val v2Vals = Values()(v2).toList
        val v3Vals = Values()(v3).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        val v24 = v2Vals indexOf 4
        val v25 = v2Vals indexOf 5
        val v314 = v3Vals indexOf List(1, 4)
        val v315 = v3Vals indexOf List(1, 5)
        val v324 = v3Vals indexOf List(2, 4)
        val v325 = v3Vals indexOf List(2, 5)
        val v334 = v3Vals indexOf List(3, 4)
        val v335 = v3Vals indexOf List(3, 5)

        factor.get(List(v314, v11, v24)) should equal(1.0)
        factor.get(List(v315, v11, v25)) should equal(1.0)
        factor.get(List(v324, v12, v24)) should equal(1.0)
        factor.get(List(v325, v12, v25)) should equal(1.0)
        factor.get(List(v334, v13, v24)) should equal(1.0)
        factor.get(List(v335, v13, v25)) should equal(1.0)

        factor.get(List(v314, v11, v25)) should equal(0.0)
        factor.get(List(v315, v11, v24)) should equal(0.0)
        factor.get(List(v324, v12, v25)) should equal(0.0)
        factor.get(List(v325, v12, v24)) should equal(0.0)
        factor.get(List(v334, v13, v25)) should equal(0.0)
        factor.get(List(v335, v13, v24)) should equal(0.0)

        factor.get(List(v314, v12, v24)) should equal(0.0)
        factor.get(List(v315, v12, v25)) should equal(0.0)
        factor.get(List(v324, v13, v24)) should equal(0.0)
        factor.get(List(v325, v13, v25)) should equal(0.0)
        factor.get(List(v334, v11, v24)) should equal(0.0)
        factor.get(List(v335, v11, v25)) should equal(0.0)

        factor.get(List(v314, v12, v25)) should equal(0.0)
        factor.get(List(v315, v12, v24)) should equal(0.0)
        factor.get(List(v324, v13, v25)) should equal(0.0)
        factor.get(List(v325, v13, v24)) should equal(0.0)
        factor.get(List(v334, v11, v25)) should equal(0.0)
        factor.get(List(v335, v11, v24)) should equal(0.0)

        factor.get(List(v314, v13, v24)) should equal(0.0)
        factor.get(List(v315, v13, v25)) should equal(0.0)
        factor.get(List(v324, v11, v24)) should equal(0.0)
        factor.get(List(v325, v11, v25)) should equal(0.0)
        factor.get(List(v334, v12, v24)) should equal(0.0)
        factor.get(List(v335, v12, v25)) should equal(0.0)

        factor.get(List(v314, v13, v25)) should equal(0.0)
        factor.get(List(v315, v13, v24)) should equal(0.0)
        factor.get(List(v324, v11, v25)) should equal(0.0)
        factor.get(List(v325, v11, v24)) should equal(0.0)
        factor.get(List(v334, v12, v25)) should equal(0.0)
        factor.get(List(v335, v12, v24)) should equal(0.0)
      }
    }

    "given a non-trivial condition and constraint" should {
      "produce the correct constraint factors" in {
        Universe.createNew()
        val v1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
        v1.setCondition((i: Int) => i != 2)
        v1.setConstraint(((i: Int) => i.toDouble))
        val List(condFactor, constrFactor, _) = ProbFactor.make(v1)
        val v1Vals = Values()(v1).toList
        val v11 = v1Vals indexOf 1
        val v12 = v1Vals indexOf 2
        val v13 = v1Vals indexOf 3
        condFactor.get(List(v11)) should equal(1.0)
        condFactor.get(List(v12)) should equal(0.0)
        condFactor.get(List(v13)) should equal(1.0)
        constrFactor.get(List(v11)) should equal(1.0)
        constrFactor.get(List(v12)) should equal(2.0)
        constrFactor.get(List(v13)) should equal(3.0)
      }
    }
  }

  "Making a factor for a dependent universe" should {
    "produce a correct dependent factor" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val dependentUniverse = new Universe(List(x, y))
      val u1 = Uniform(0.0, 1.0)("", dependentUniverse)
      val u2 = Uniform(0.0, 2.0)("", dependentUniverse)
      val a = CachingChain(x, y, (x: Boolean, y: Int) => if (x || y < 2) u1; else u2)("a", dependentUniverse)
      val evidence = List(NamedEvidence("a", Condition((d: Double) => d < 0.5)))
      val factor =
        ProbFactor.makeDependentFactor(Universe.universe, dependentUniverse, () => ProbEvidenceSampler.computeProbEvidence(20000, evidence)(dependentUniverse))
      val xVar = Variable(x)
      val yVar = Variable(y)
      val variables = factor.variables
      variables.toSet should equal(Set(xVar, yVar))
      val xIndex = variables indexOf xVar
      val yIndex = variables indexOf yVar
      val xFalse = xVar.range indexOf false
      val xTrue = xVar.range indexOf true
      val y1 = yVar.range indexOf 1
      val y2 = yVar.range indexOf 2
      val y3 = yVar.range indexOf 3
      // If x is true or y is 1, pe is 0.5; if both false, 0.25.
      if (xIndex == 0) {
        factor.get(List(xFalse, y2)) should be(0.25 plusOrMinus 0.01)
        factor.get(List(xFalse, y3)) should be(0.25 plusOrMinus 0.01)
        factor.get(List(xFalse, y1)) should be(0.5 plusOrMinus 0.01)
        factor.get(List(xTrue, y1)) should be(0.5 plusOrMinus 0.01)
        factor.get(List(xTrue, y2)) should be(0.5 plusOrMinus 0.01)
        factor.get(List(xTrue, y3)) should be(0.5 plusOrMinus 0.01)
      } else {
        factor.get(List(y2, xFalse)) should be(0.25 plusOrMinus 0.01)
        factor.get(List(y3, xFalse)) should be(0.25 plusOrMinus 0.01)
        factor.get(List(y1, xTrue)) should be(0.5 plusOrMinus 0.01)
        factor.get(List(y1, xFalse)) should be(0.5 plusOrMinus 0.01)
        factor.get(List(y2, xFalse)) should be(0.5 plusOrMinus 0.01)
        factor.get(List(y3, xFalse)) should be(0.5 plusOrMinus 0.01)
      }
    }
  }
}
