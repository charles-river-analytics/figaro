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

import org.scalatest.Matchers
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination
import scala.collection.mutable.ListBuffer
import com.cra.figaro.algorithm.factored.factors.factory.Factory

class FactorTest extends WordSpec with Matchers with PrivateMethodTester {

  "A variable for an element" should {
    "have range equal to the element's values" in {
      Universe.createNew()
      val e1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val xs = Values()(e1)
      val v1 = Variable(e1)
      for { x <- v1.range } { xs should contain(x.value) }
      //for { x <- xs } { v1.range should contain(x) }
    }

    "always be equal to another variable for the same element" in {
      Universe.createNew()
      val e1 = Flip(0.2)
      Values()(e1)
      val v1 = Variable(e1)
      val v2 = Variable(e1)
      v1 should equal(v2)
    }

    "always contain the same id even if the Variable cache is cleared" in {
      Universe.createNew()
      val e1 = Flip(0.2)
      Values()(e1)
      val v1 = Variable(e1).id
      Variable.clearCache
      LazyValues.clear(Universe.universe)
      Values()(e1)
      val v2 = Variable(e1).id
      v1 should equal(v2)
    }

    "always be equal to a variable with the same id" in {
      Universe.createNew()
      val e1 = Flip(0.2)
      Values()(e1)
      val v1 = Variable(e1)
      val v2 = new Variable(ValueSet.withStar(Set[Boolean]())) { override val id = v1.id }
      v1 == v2 should equal(true)
    }

    "be different to a variable for a different element with the same definition" in {
      Universe.createNew()
      val e1 = Flip(0.2)
      val e2 = Flip(0.2)
      Values()(e1)
      Values()(e2)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      v1 should not equal (v2)
    }
  }

  "A factor" when {
    "get the same value for a given set of variable indices as was last set" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      Values()(e1)
      Values()(e2)
      Values()(e3)
      Values()(e4)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = Factory.defaultFactor[Double](List(v1, v2, v3, v4), List())
      val indices = List(1, 0, 2, 1)
      f.set(indices, 0.2)
      f.set(indices, 0.3)
      f.get(indices) should equal(0.3)
    }

    /*
    "get updated set of factors for an element when the factors have been updated" in {
      Universe.createNew()
      val v1 = Flip(0.5)
      Values()(v1)
      val f1 = Factory.makeFactorsForElement(v1)(0)
      val f1mod = f1.mapTo((d: Double) => 2.0 * d)
      Factory.updateFactor(v1, List(f1mod))
      Factory.makeFactorsForElement(v1)(0).get(List(0)) should equal(f1mod.get(List(0)))
    }
    * 
    */

    //    "have the first index List be all zeros" in {
    //      Universe.createNew()
    //      val e1 = Flip(0.1)
    //      val e2 = Constant(8)
    //      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
    //      val e4 = Flip(0.7)
    //      Values()(e1)
    //      Values()(e2)
    //      Values()(e3)
    //      Values()(e4)
    //      val v1 = Variable(e1)
    //      val v2 = Variable(e2)
    //      val v3 = Variable(e3)
    //      val v4 = Variable(e4)
    //      val f = Factory.defaultFactor[Double](List(v1, v2, v3, v4))
    //      f.firstIndices should equal(List(0, 0, 0, 0))
    //    }

    "have the next index List carry and add correctly" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      Values()(e1)
      Values()(e2)
      Values()(e3)
      Values()(e4)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = Factory.defaultFactor[Double](List(v1, v2, v3, v4), List())
      val ia = List(1, 0, 1, 1)
      val indices = f.getIndices
      val ar = indices.nextIndices(ia).get
      ar should equal(List(1, 0, 2, 0))
    }

    "produce None when the index Lists are exhausted" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      Values()(e1)
      Values()(e2)
      Values()(e3)
      Values()(e4)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = Factory.defaultFactor[Double](List(v1, v2, v3, v4), List())
      val ia = List(1, 0, 2, 1)
      val indices = f.getIndices
      indices.nextIndices(ia) should equal(None)
    }

    "compute the union of variables in two factors and the correct index maps when calling unionVars" in {
      Universe.createNew()
      val e1 = Flip(0.1)
      val e2 = Constant(8)
      val e3 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e4 = Flip(0.7)
      val e5 = Constant('a)
      val e6 = Select(0.1 -> 1.5, 0.9 -> 2.5)
      Values()(e1)
      Values()(e2)
      Values()(e3)
      Values()(e4)
      Values()(e5)
      Values()(e6)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val v5 = Variable(e5)
      val v6 = Variable(e6)
      val f = Factory.defaultFactor[Double](List(v1, v2, v3, v4), List())
      val g = Factory.defaultFactor[Double](List(v5, v3, v2, v6), List())
      val unionVars = PrivateMethod[(List[Variable[_]], List[Variable[_]], List[Int], List[Int])]('unionVars)
      val (parents, output, indexMap1, indexMap2) = f invokePrivate unionVars(g)
      val union = parents ::: output
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
        Values()(e1)
        Values()(e2)
        Values()(e3)
        Values()(e4)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val f = Factory.defaultFactor[Double](List(v1, v2, v3), List())
        val g = Factory.defaultFactor[Double](List(v4, v3), List())
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
        val h = f.product(g)
        h.variables should equal(List(v1, v2, v3, v4))
        h.get(List(0, 0, 0, 0)) should be(0.0 +- 0.0001)
        h.get(List(1, 0, 0, 0)) should be(0.06 +- 0.0001)
        h.get(List(2, 0, 0, 0)) should be(0.12 +- 0.0001)
        h.get(List(0, 0, 1, 0)) should be(0.24 +- 0.0001)
        h.get(List(1, 0, 1, 0)) should be(0.32 +- 0.0001)
        h.get(List(2, 0, 1, 0)) should be(0.4 +- 0.0001)
        h.get(List(0, 0, 0, 1)) should be(0.0 +- 0.0001)
        h.get(List(1, 0, 0, 1)) should be(0.07 +- 0.0001)
        h.get(List(2, 0, 0, 1)) should be(0.14 +- 0.0001)
        h.get(List(0, 0, 1, 1)) should be(0.27 +- 0.0001)
        h.get(List(1, 0, 1, 1)) should be(0.36 +- 0.0001)
        h.get(List(2, 0, 1, 1)) should be(0.45 +- 0.0001)
      }
    }

    "calling sumOver on a variable" should {
      "return the sum over the variable of the factor" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        Values()(e1)
        Values()(e2)
        Values()(e3)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = Factory.defaultFactor[Double](List(v1, v2, v3), List())
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.sumOver(v3)
        g.variables should equal(List(v1, v2))
        g.get(List(0, 0)) should be(0.3 +- 0.0001)
        g.get(List(1, 0)) should be(0.5 +- 0.0001)
        g.get(List(2, 0)) should be(0.7 +- 0.0001)
      }

      "return itself if the variable not in the factor" in {
        Universe.createNew()
        val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
        val e2 = Constant(8)
        val e3 = Flip(0.1)
        Values()(e1)
        Values()(e2)
        Values()(e3)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = Factory.defaultFactor[Double](List(v1, v2), List())
        f.set(List(0, 0), 0.0)
        f.set(List(1, 0), 0.2)
        f.set(List(2, 0), 0.4)
        val g = f.sumOver(v3)
        g.variables should equal(f.variables)
        for { indices <- f.getIndices } {
          g.get(indices) should equal(f.get(indices))
        }
      }

      "return a factor with all columns of the variable removed, ignoring rows in which " +
        "the variable has different values in different columns" in {
          Universe.createNew()
          val e1 = Flip(0.9)
          val e2 = Select(0.2 -> 1, 0.8 -> 2)
          Values()(e1)
          Values()(e2)
          val v1 = Variable(e1)
          val v2 = Variable(e2)
          val f = Factory.defaultFactor[Double](List(v1, v2, v1), List())
          f.set(List(0, 0, 0), 0.1)
          f.set(List(1, 0, 0), 0.2)
          f.set(List(0, 1, 0), 0.3)
          f.set(List(1, 1, 0), 0.4)
          f.set(List(0, 0, 1), 0.5)
          f.set(List(1, 0, 1), 0.6)
          f.set(List(0, 1, 1), 0.7)
          f.set(List(1, 1, 1), 0.8)
          val g = f.sumOver(v1)
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
        Values()(e1)
        Values()(e2)
        Values()(e3)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = Factory.defaultFactor[Double](List(v1, v2, v3), List())
        f.set(List(0, 0, 0), 0.6)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.recordArgMax(v3.asInstanceOf[Variable[Any]],
          (x: Double, y: Double) => x < y)
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
        Values()(e1)
        Values()(e2)
        Values()(e3)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = Factory.defaultFactor[Double](List(v1, v2, v3), List())
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.marginalizeTo(v3)
        g.variables should equal(List(v3))
        val p1 = 0.0 + 0.1 + 0.2
        val p2 = 0.3 + 0.4 + 0.5
        g.get(List(0)) should be(p1 +- 0.000001)
        g.get(List(1)) should be(p2 +- 0.000001)
      }
    }
  }

  "after marginalizing to two variables" should {
    "return the marginal distribution over the variables" in {
      Universe.createNew()
      val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e2 = Flip(0.5)
      val e3 = Flip(0.1)
      Values()(e1)
      Values()(e2)
      Values()(e3)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val f = Factory.defaultFactor[Double](List(v1, v2, v3), List())
      f.set(List(0, 0, 0), 0.0)
      f.set(List(1, 0, 0), 0.05)
      f.set(List(2, 0, 0), 0.1)
      f.set(List(0, 0, 1), 0.15)
      f.set(List(1, 0, 1), 0.2)
      f.set(List(2, 0, 1), 0.25)
      f.set(List(0, 1, 0), 0.0)
      f.set(List(1, 1, 0), 0.05)
      f.set(List(2, 1, 0), 0.1)
      f.set(List(0, 1, 1), 0.15)
      f.set(List(1, 1, 1), 0.2)
      f.set(List(2, 1, 1), 0.25)
      val g = f.marginalizeTo(v1, v3)
      g.variables should equal(List(v1, v3))
      g.get(List(0, 0)) should be(0.0 +- 0.000001)
      g.get(List(1, 0)) should be(0.1 +- 0.000001)
      g.get(List(2, 0)) should be(0.2 +- 0.000001)
      g.get(List(0, 1)) should be(0.3 +- 0.000001)
      g.get(List(1, 1)) should be(0.4 +- 0.000001)
      g.get(List(2, 1)) should be(0.5 +- 0.000001)
    }
  }

  "after deduplicating a factor" should {
    "have no repeated variables" in {
      Universe.createNew()
      val e1 = Select(0.2 -> "a", 0.3 -> "b", 0.5 -> "c")
      val e2 = Flip(0.3)
      Values()(e1)
      Values()(e2)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val f = Factory.defaultFactor[Double](List(v1, v2, v2), List())
      f.set(List(0, 0, 0), 0.06)
      f.set(List(0, 0, 1), 0.25)
      f.set(List(0, 1, 0), 0.44)
      f.set(List(0, 1, 1), 0.25)
      f.set(List(1, 0, 0), 0.15)
      f.set(List(1, 0, 1), 0.2)
      f.set(List(1, 1, 0), 0.15)
      f.set(List(1, 1, 1), 0.5)
      f.set(List(2, 0, 0), 0.1)
      f.set(List(2, 0, 1), 0.25)
      f.set(List(2, 1, 0), 0.4)
      f.set(List(2, 1, 1), 0.25)

      val g = f.deDuplicate()

      g.variables.size should be(2)
      g.variables.contains(v1) should be(true)
      g.variables.contains(v2) should be(true)

      if (g.variables.indexOf(v1) == 0) {
        g.get(List(0, 0)) should be(0.06 +- 0.000001)
        g.get(List(0, 1)) should be(0.25 +- 0.000001)
        g.get(List(1, 0)) should be(0.15 +- 0.000001)
        g.get(List(1, 1)) should be(0.5 +- 0.000001)
        g.get(List(2, 0)) should be(0.1 +- 0.000001)
        g.get(List(2, 1)) should be(0.25 +- 0.000001)
      } else {
        g.get(List(0, 0)) should be(0.06 +- 0.000001)
        g.get(List(1, 0)) should be(0.25 +- 0.000001)
        g.get(List(0, 1)) should be(0.15 +- 0.000001)
        g.get(List(1, 1)) should be(0.5 +- 0.000001)
        g.get(List(0, 2)) should be(0.1 +- 0.000001)
        g.get(List(1, 2)) should be(0.25 +- 0.000001)
      }

    }
  }
  "Making factors from an element" when {

    "given a constant" should {
      "produce a single factor with one entry whose value is 1.0" in {
        Universe.createNew()
        val v1 = Constant(7)
        Values()(v1)
        val List(factor) = Factory.makeFactorsForElement(v1)
        factor.get(List(0)) should equal(1.0)
      }
    }

    "given a simple flip" should {
      "produce a single factor in which the first entry is the probability of true " +
        "and the second entry is the probability of false" in {
          Universe.createNew()
          val v1 = Flip(0.3)
          Values()(v1)
          val List(factor) = Factory.makeFactorsForElement(v1)
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
          Values()(v2)
          Variable(v1)
          Variable(v2)
          val List(factor) = Factory.makeFactorsForElement(v2)
          val vals = Variable(v1).range
          val i1 = vals.indexOf(Regular(0.1))
          val i2 = vals.toList.indexOf(Regular(0.3))
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
        Values()(v1)
        val List(factor) = Factory.makeFactorsForElement(v1)
        val vals = Variable(v1).range
        val i1 = vals.indexOf(Regular(1))
        val i0 = vals.indexOf(Regular(0))
        val i2 = vals.indexOf(Regular(2))
        val i5 = vals.indexOf(Regular(5))
        val i4 = vals.indexOf(Regular(4))
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
          Values()(v3)
          Universe.universe.activeElements.foreach(Variable(_))
          val List(factor) = Factory.makeFactorsForElement(v3)
          val v1Vals = Variable(v1).range
          val v2Vals = Variable(v2).range
          val v3Vals = Variable(v3).range
          val v102 = v1Vals.indexOf(Regular(0.2))
          val v108 = v1Vals.indexOf(Regular(0.8))
          val v204 = v2Vals.indexOf(Regular(0.4))
          val v206 = v2Vals.indexOf(Regular(0.6))
          val v31 = v3Vals.indexOf(Regular(1))
          val v32 = v3Vals.indexOf(Regular(2))
          val v34 = v3Vals.indexOf(Regular(4))
          val v35 = v3Vals.indexOf(Regular(5))
          val v33 = v3Vals.indexOf(Regular(3))
          def makeIndices(a: List[Int]): List[Int] = {
            val result: Array[Int] = Array.ofDim(a.size)
            result(v31) = a(1)
            result(v32) = a(2)
            result(v33) = a(3)
            result(v34) = a(4)
            result(v35) = a(5)
            result(5) = a(0)

            result.toList
          }
          factor.get(makeIndices(List(v31, v102, v204, 0, 0, 0))) should be(0.2 / 1.5 +- 0.01)
          factor.get(makeIndices(List(v32, v102, v204, 0, 0, 0))) should be(0.4 / 1.5 +- 0.01)
          factor.get(makeIndices(List(v31, v108, v204, 0, 0, 0))) should be(0.8 / 2.1 +- 0.01)
          factor.get(makeIndices(List(v32, v108, v204, 0, 0, 0))) should be(0.4 / 2.1 +- 0.01)
          factor.get(makeIndices(List(v31, v102, v206, 0, 0, 0))) should be(0.2 / 1.7 +- 0.01)
          factor.get(makeIndices(List(v32, v102, v206, 0, 0, 0))) should be(0.6 / 1.7 +- 0.01)
          factor.get(makeIndices(List(v31, v108, v206, 0, 0, 0))) should be(0.8 / 2.3 +- 0.01)
          factor.get(makeIndices(List(v32, v108, v206, 0, 0, 0))) should be(0.6 / 2.3 +- 0.01)
        }
    }

    "given a simple dist" should {
      "produce a list of factors, one for each outcome and one representing the choice over outcomes; " +
        "the factor for an outcome matches the outcome value to the dist value" in {
          Universe.createNew()
          val v1 = Flip(0.2)
          val v2 = Constant(false)
          val v3 = Dist(0.3 -> v1, 0.7 -> v2)
          Values()(v3)
          val v1Vals = Variable(v1).range
          val v3Vals = Variable(v3).range
          val v1TrueIndex = v1Vals.indexOf(Regular(true))
          val v1FalseIndex = v1Vals.indexOf(Regular(false))
          val v3TrueIndex = v3Vals.indexOf(Regular(true))
          val v3FalseIndex = v3Vals.indexOf(Regular(false))
          val v1Index = v3.outcomes.indexOf(v1)
          val v2Index = v3.outcomes.indexOf(v2)
          Universe.universe.activeElements.foreach(Variable(_))
          val selectFactor :: outcomeFactors = Factory.makeFactorsForElement(v3)
          outcomeFactors.size should equal(3)
          val v1Factor = outcomeFactors(1+v1Index)
          val v2Factor = outcomeFactors(1+v2Index)
          val pairFactor = outcomeFactors(0)
          val pairRange: List[List[Any]] = pairFactor.output.head.range.map(_.value.asInstanceOf[List[Any]])
          selectFactor.get(List(v1Index)) should equal(0.3)
          selectFactor.get(List(v2Index)) should equal(0.7)
          val index1 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == v1Index && p(1).asInstanceOf[Regular[Boolean]].value == true)
          val index2 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == v1Index && p(1).asInstanceOf[Regular[Boolean]].value == false)
          v1Factor.get(List(index1, v3TrueIndex)) should equal(1.0)
          v1Factor.get(List(index2, v3TrueIndex)) should equal(0.0)
          v1Factor.get(List(index1, v3FalseIndex)) should equal(0.0)
          v1Factor.get(List(index2, v3FalseIndex)) should equal(1.0)
          for { i <- 0 to 1; j <- 0 to 1 } {
            val ind = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == v2Index && p(1).asInstanceOf[Regular[Boolean]].value == v3Vals(i).value)
            v1Factor.get(List(ind, j)) should equal(1.0)
          }
          val v2index1 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == v2Index && p(1).asInstanceOf[Regular[Boolean]].value == true)
          val v2index2 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == v2Index && p(1).asInstanceOf[Regular[Boolean]].value == false)
          v2Factor.get(List(v2index1, v3FalseIndex)) should equal(1.0)
          v2Factor.get(List(v2index1, v3TrueIndex)) should equal(0.0)
          for { i <- 0 to 1 } {
            val ind = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == v1Index && p(1).asInstanceOf[Regular[Boolean]].value == v3Vals(i).value)
            v2Factor.get(List(ind, i)) should equal(1.0)
          }
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
          Values()(v5)
          val v1Vals = Variable(v1).range
          val v2Vals = Variable(v2).range
          val v3Vals = Variable(v3).range
          val v4Vals = Variable(v4).range
          val v5Vals = Variable(v5).range
          val v3Index = v5.outcomes.indexOf(v3)
          val v4Index = v5.outcomes.indexOf(v4)
          val v102 = v1Vals.indexOf(Regular(0.2))
          val v108 = v1Vals.indexOf(Regular(0.8))
          val v204 = v2Vals.indexOf(Regular(0.4))
          val v206 = v2Vals.indexOf(Regular(0.6))
          val v3f = v3Vals.indexOf(Regular(false))
          val v3t = v3Vals.indexOf(Regular(true))
          val v5f = v5Vals.indexOf(Regular(false))
          val v5t = v5Vals.indexOf(Regular(true))
          Universe.universe.activeElements.foreach(Variable(_))
          val selectFactor :: outcomeFactors = Factory.makeFactorsForElement(v5)
          outcomeFactors.size should equal(3)
          val v1Factor = outcomeFactors(1+v3Index)
          val v2Factor = outcomeFactors(1+v4Index)
          val pairFactor = outcomeFactors(0)
          val pairRange: List[List[Any]] = pairFactor.output.head.range.map(_.value.asInstanceOf[List[Any]])          
          selectFactor.get(List(v102, v204, 0)) should be(0.2 / 0.6 +- 0.0001)
          selectFactor.get(List(v102, v204, 1)) should be(0.4 / 0.6 +- 0.0001)
          selectFactor.get(List(v102, v206, 0)) should be(0.2 / 0.8 +- 0.0001)
          selectFactor.get(List(v102, v206, 1)) should be(0.6 / 0.8 +- 0.0001)
          selectFactor.get(List(v108, v204, 0)) should be(0.8 / 1.2 +- 0.0001)
          selectFactor.get(List(v108, v204, 1)) should be(0.4 / 1.2 +- 0.0001)
          selectFactor.get(List(v108, v206, 0)) should be(0.8 / 1.4 +- 0.0001)
          selectFactor.get(List(v108, v206, 1)) should be(0.6 / 1.4 +- 0.0001)
          val v1index1 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == 0 && p(1).asInstanceOf[Regular[Boolean]].value == true)
          val v1index2 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == 0 && p(1).asInstanceOf[Regular[Boolean]].value == false)
          v1Factor.get(List(v1index1, v5t)) should equal(1.0)
          v1Factor.get(List(v1index2, v5t)) should equal(0.0)
          v1Factor.get(List(v1index1, v5f)) should equal(0.0)
          v1Factor.get(List(v1index2, v5f)) should equal(1.0)
          for { i <- 0 to 1; j <- 0 to 1 } {
            val ind = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == 1 && p(1).asInstanceOf[Regular[Boolean]].value == v5Vals(i).value)
            v1Factor.get(List(ind, j)) should equal(1.0)
          }
          val v2index1 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == 1 && p(1).asInstanceOf[Regular[Boolean]].value == v5Vals(0).value)
          //val v2index2 = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == 1 && p(1).asInstanceOf[Regular[Boolean]].value == fals)
          v2Factor.get(List(v2index1, v5f)) should equal(1.0)
          v2Factor.get(List(v2index1, v5t)) should equal(0.0)
          for { i <- 0 to 0; j <- 0 to 1 } {
            val ind = pairRange.indexWhere(p => p(0).asInstanceOf[Regular[Int]].value == 0 && p(1).asInstanceOf[Regular[Boolean]].value == v5Vals(i).value)
            v2Factor.get(List(ind, j)) should equal(1.0)
          }
        }
    }

    "given an atomic not in the factor" should {
      "automatically sample the element" in {
        Universe.createNew()
        val v1 = Normal(0.0, 1.0)
        Values()(v1)
        val factor = Factory.makeFactorsForElement(v1)
        factor(0).size should equal(ParticleGenerator.defaultNumSamplesFromAtomics)
        factor(0).get(List(0)) should equal(1.0 / ParticleGenerator.defaultNumSamplesFromAtomics)
      }

      "correctly create factors for continuous elements through chains" in {
        val uni = Universe.createNew()
        val elem = If(Flip(0.3), Uniform(0.0, 1.0), Uniform(1.0, 2.0))
        ParticleGenerator(uni)
        val alg = VariableElimination(elem)
        alg.start()
        alg.distribution(elem).toList.size should be(14)

      }
    }

    //    "given a chain" should {
    //      "produce a conditional selector for each parent value" in {
    //        Universe.createNew()
    //        val v1 = Flip(0.2)
    //        val v2 = Select(0.1 -> 1, 0.9 -> 2)
    //        val v3 = Constant(3)
    //        val v4 = Chain(v1, (b: Boolean) => if (b) v2; else v3)
    //        Values()(v4)
    //        val v1Vals = Variable(v1).range
    //        val v2Vals = Variable(v2).range
    //        val v4Vals = Variable(v4).range
    //        val v1t = v1Vals indexOf Regular(true)
    //        val v1f = v1Vals indexOf Regular(false)
    //        val v21 = v2Vals indexOf Regular(1)
    //        val v22 = v2Vals indexOf Regular(2)
    //        val v41 = v4Vals indexOf Regular(1)
    //        val v42 = v4Vals indexOf Regular(2)
    //        val v43 = v4Vals indexOf Regular(3)
    //
    //        val factor = Factory.makeFactorsForElement(v4)
    //        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
    //
    //        v4Factor.get(List(v1t, v21, 0, v41)) should equal(1.0)
    //        v4Factor.get(List(v1t, v22, 0, v41)) should equal(0.0)
    //        v4Factor.get(List(v1t, v21, 0, v42)) should equal(0.0)
    //        v4Factor.get(List(v1t, v22, 0, v42)) should equal(1.0)
    //        v4Factor.get(List(v1t, v21, 0, v43)) should equal(0.0)
    //        v4Factor.get(List(v1t, v22, 0, v43)) should equal(0.0)
    //        v4Factor.get(List(v1f, v21, 0, v41)) should equal(0.0)
    //        v4Factor.get(List(v1f, v22, 0, v41)) should equal(0.0)
    //        v4Factor.get(List(v1f, v21, 0, v42)) should equal(0.0)
    //        v4Factor.get(List(v1f, v22, 0, v42)) should equal(0.0)
    //        v4Factor.get(List(v1f, v21, 0, v43)) should equal(1.0)
    //        v4Factor.get(List(v1f, v22, 0, v43)) should equal(1.0)
    //
    //      }
    //
    //      "produce a conditional selector for each non-temporary parent value" in {
    //        Universe.createNew()
    //        val v1 = Flip(0.2)
    //        val v4 = Chain(v1, (b: Boolean) => if (b) Select(0.1 -> 1, 0.9 -> 2); else Constant(3))
    //        Values()(v4)
    //        val v1Vals = Variable(v1).range
    //        val v4Vals = Variable(v4).range
    //
    //        val v1t = v1Vals indexOf Regular(true)
    //        val v1f = v1Vals indexOf Regular(false)
    //        val v41 = v4Vals indexOf Regular(1)
    //        val v42 = v4Vals indexOf Regular(2)
    //        val v43 = v4Vals indexOf Regular(3)
    //
    //        val factor = Factory.makeFactorsForElement(v4)
    //        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
    //
    //        v4Factor.get(List(v1t, v41)) should equal(0.1)
    //        v4Factor.get(List(v1t, v42)) should equal(0.9)
    //        v4Factor.get(List(v1t, v43)) should equal(0.0)
    //        v4Factor.get(List(v1f, v41)) should equal(0.0)
    //        v4Factor.get(List(v1f, v42)) should equal(0.0)
    //        v4Factor.get(List(v1f, v43)) should equal(1.0)
    //      }
    //    }

    //    "given a CPD with one argument" should {
    //      "produce a single factor with a case for each parent value" in {
    //        Universe.createNew()
    //        val v1 = Flip(0.2)
    //
    //        val v2 = CPD(v1, false -> Flip(0.1), true -> Flip(0.7))
    //        Values()(v2)
    //
    //        val v1Vals = Variable(v1).range
    //        val v2Vals = Variable(v2).range
    //
    //        val v1t = v1Vals indexOf Regular(true)
    //        val v1f = v1Vals indexOf Regular(false)
    //        val v2t = v2Vals indexOf Regular(true)
    //        val v2f = v2Vals indexOf Regular(false)
    //        val v3t = 0
    //        val v3f = 1
    //        val v4t = 0
    //        val v4f = 1
    //
    //        val factor = Factory.makeFactorsForElement(v2)
    //        val List(v2Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
    //
    //        v2Factor.get(List(v1t, v3t, v4t, v2t)) should equal(1.0)
    //        v2Factor.get(List(v1t, v3t, v4f, v2t)) should equal(1.0)
    //        v2Factor.get(List(v1t, v3f, v4t, v2t)) should equal(0.0)
    //        v2Factor.get(List(v1t, v3f, v4f, v2t)) should equal(0.0)
    //        v2Factor.get(List(v1t, v3t, v4t, v2f)) should equal(0.0)
    //        v2Factor.get(List(v1t, v3t, v4f, v2f)) should equal(0.0)
    //        v2Factor.get(List(v1t, v3f, v4t, v2f)) should equal(1.0)
    //        v2Factor.get(List(v1t, v3f, v4f, v2f)) should equal(1.0)
    //        v2Factor.get(List(v1f, v3t, v4t, v2t)) should equal(1.0)
    //        v2Factor.get(List(v1f, v3t, v4f, v2t)) should equal(0.0)
    //        v2Factor.get(List(v1f, v3f, v4t, v2t)) should equal(1.0)
    //        v2Factor.get(List(v1f, v3f, v4f, v2t)) should equal(0.0)
    //        v2Factor.get(List(v1f, v3t, v4t, v2f)) should equal(0.0)
    //        v2Factor.get(List(v1f, v3t, v4f, v2f)) should equal(1.0)
    //        v2Factor.get(List(v1f, v3f, v4t, v2f)) should equal(0.0)
    //        v2Factor.get(List(v1f, v3f, v4f, v2f)) should equal(1.0)
    //      }
    //    }

    "given an apply of one argument" should {
      "produce a factor that matches the argument to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Apply(v1, (i: Int) => i % 2)
        Values()(v2)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v20 = v2Vals indexOf Regular(0)
        val v21 = v2Vals indexOf Regular(1)
        val List(factor) = Factory.makeFactorsForElement(v2)
        factor.contains(List(v11, v20)) should equal(false)
        factor.get(List(v11, v21)) should equal(1.0)
        factor.get(List(v12, v20)) should equal(1.0)
        factor.contains(List(v12, v21)) should equal(false)
        factor.contains(List(v13, v20)) should equal(false)
        factor.get(List(v13, v21)) should equal(1.0)
        factor.contents.size should equal(3)
      }
    }

    "given an apply of two arguments" should {
      "produce a factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 2, 0.5 -> 3)
        val v3 = Apply(v1, v2, (i: Int, j: Int) => i % j)
        Values()(v3)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v3Vals = Variable(v3).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v22 = v2Vals indexOf Regular(2)
        val v23 = v2Vals indexOf Regular(3)
        val v30 = v3Vals indexOf Regular(0)
        val v31 = v3Vals indexOf Regular(1)
        val v32 = v3Vals indexOf Regular(2)
        val List(factor) = Factory.makeFactorsForElement(v3)
        factor.contains(List(v11, v22, v30)) should equal(false)
        factor.get(List(v11, v22, v31)) should equal(1.0)
        factor.contains(List(v11, v22, v32)) should equal(false)
        factor.contains(List(v11, v23, v30)) should equal(false)
        factor.get(List(v11, v23, v31)) should equal(1.0)
        factor.contains(List(v11, v23, v32)) should equal(false)
        factor.get(List(v12, v22, v30)) should equal(1.0)
        factor.contains(List(v12, v22, v31)) should equal(false)
        factor.contains(List(v12, v22, v32)) should equal(false)
        factor.contains(List(v12, v23, v30)) should equal(false)
        factor.contains(List(v12, v23, v31)) should equal(false)
        factor.get(List(v12, v23, v32)) should equal(1.0)
        factor.contains(List(v13, v22, v30)) should equal(false)
        factor.get(List(v13, v22, v31)) should equal(1.0)
        factor.contains(List(v13, v22, v32)) should equal(false)
        factor.get(List(v13, v23, v30)) should equal(1.0)
        factor.contains(List(v13, v23, v31)) should equal(false)
        factor.contains(List(v13, v23, v32)) should equal(false)
        factor.contents.size should equal(6)
      }
    }

    "given an apply of three arguments" should {
      "produce a factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4: Apply3[Int, Int, Int, Int] = Apply(v1, v2, v3, (i: Int, j: Int, k: Int) => i % (j + k))
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v3Vals = Variable(v3).range
        val v4Vals = Variable(v4).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v31 = v3Vals indexOf Regular(1)
        val v40 = v4Vals indexOf Regular(0)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val List(factor) = Factory.makeFactorsForElement(v4)
        factor.contains(List(v11, v21, v31, v40)) should equal(false)
        factor.get(List(v11, v21, v31, v41)) should equal(1.0)
        factor.contains(List(v11, v21, v31, v42)) should equal(false)
        factor.contains(List(v11, v22, v31, v40)) should equal(false)
        factor.get(List(v11, v22, v31, v41)) should equal(1.0)
        factor.contains(List(v11, v22, v31, v42)) should equal(false)
        factor.get(List(v12, v21, v31, v40)) should equal(1.0)
        factor.contains(List(v12, v21, v31, v41)) should equal(false)
        factor.contains(List(v12, v21, v31, v42)) should equal(false)
        factor.contains(List(v12, v22, v31, v40)) should equal(false)
        factor.contains(List(v12, v22, v31, v41)) should equal(false)
        factor.get(List(v12, v22, v31, v42)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v40)) should equal(false)
        factor.get(List(v13, v21, v31, v41)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v42)) should equal(false)
        factor.get(List(v13, v22, v31, v40)) should equal(1.0)
        factor.contains(List(v13, v22, v31, v41)) should equal(false)
        factor.contains(List(v13, v22, v31, v42)) should equal(false)
        factor.contents.size should equal(6)
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
        Values()(v5)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v3Vals = Variable(v3).range
        val v4Vals = Variable(v4).range
        val v5Vals = Variable(v5).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v31 = v3Vals indexOf Regular(1)
        val v4true = v4Vals indexOf Regular(true)
        val v4false = v4Vals indexOf Regular(false)
        val v50 = v5Vals indexOf Regular(0)
        val v51 = v5Vals indexOf Regular(1)
        val v52 = v5Vals indexOf Regular(2)
        val List(factor) = Factory.makeFactorsForElement(v5)
        factor.contains(List(v11, v21, v31, v4false, v50)) should equal(false)
        factor.get(List(v11, v21, v31, v4false, v51)) should equal(1.0)
        factor.contains(List(v11, v21, v31, v4false, v52)) should equal(false)
        factor.contains(List(v11, v22, v31, v4false, v50)) should equal(false)
        factor.get(List(v11, v22, v31, v4false, v51)) should equal(1.0)
        factor.contains(List(v11, v22, v31, v4false, v52)) should equal(false)
        factor.get(List(v12, v21, v31, v4false, v50)) should equal(1.0)
        factor.contains(List(v12, v21, v31, v4false, v51)) should equal(false)
        factor.contains(List(v12, v21, v31, v4false, v52)) should equal(false)
        factor.contains(List(v12, v22, v31, v4false, v50)) should equal(false)
        factor.contains(List(v12, v22, v31, v4false, v51)) should equal(false)
        factor.get(List(v12, v22, v31, v4false, v52)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v4false, v50)) should equal(false)
        factor.get(List(v13, v21, v31, v4false, v51)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v4false, v52)) should equal(false)
        factor.get(List(v13, v22, v31, v4false, v50)) should equal(1.0)
        factor.contains(List(v13, v22, v31, v4false, v51)) should equal(false)
        factor.contains(List(v13, v22, v31, v4false, v52)) should equal(false)

        factor.get(List(v11, v21, v31, v4true, v50)) should equal(1.0)
        factor.contains(List(v11, v21, v31, v4true, v51)) should equal(false)
        factor.contains(List(v11, v21, v31, v4true, v52)) should equal(false)
        factor.get(List(v11, v22, v31, v4true, v50)) should equal(1.0)
        factor.contains(List(v11, v22, v31, v4true, v51)) should equal(false)
        factor.contains(List(v11, v22, v31, v4true, v52)) should equal(false)
        factor.get(List(v12, v21, v31, v4true, v50)) should equal(1.0)
        factor.contains(List(v12, v21, v31, v4true, v51)) should equal(false)
        factor.contains(List(v12, v21, v31, v4true, v52)) should equal(false)
        factor.get(List(v12, v22, v31, v4true, v50)) should equal(1.0)
        factor.contains(List(v12, v22, v31, v4true, v51)) should equal(false)
        factor.contains(List(v12, v22, v31, v4true, v52)) should equal(false)
        factor.get(List(v13, v21, v31, v4true, v50)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v4true, v51)) should equal(false)
        factor.contains(List(v13, v21, v31, v4true, v52)) should equal(false)
        factor.get(List(v13, v22, v31, v4true, v50)) should equal(1.0)
        factor.contains(List(v13, v22, v31, v4true, v51)) should equal(false)
        factor.contains(List(v13, v22, v31, v4true, v52)) should equal(false)
        factor.contents.size should equal(12)
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
        Values()(v6)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v3Vals = Variable(v3).range
        val v4Vals = Variable(v4).range
        val v5Vals = Variable(v5).range
        val v6Vals = Variable(v6).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v31 = v3Vals indexOf Regular(1)
        val v4true = v4Vals indexOf Regular(true)
        val v4false = v4Vals indexOf Regular(false)
        val v5false = v5Vals indexOf Regular(false)
        val v60 = v6Vals indexOf Regular(0)
        val v61 = v6Vals indexOf Regular(1)
        val v62 = v6Vals indexOf Regular(2)
        val List(factor) = Factory.makeFactorsForElement(v6)

        factor.contains(List(v11, v21, v31, v4false, v5false, v60)) should equal(false)
        factor.get(List(v11, v21, v31, v4false, v5false, v61)) should equal(1.0)
        factor.contains(List(v11, v21, v31, v4false, v5false, v62)) should equal(false)
        factor.contains(List(v11, v22, v31, v4false, v5false, v60)) should equal(false)
        factor.get(List(v11, v22, v31, v4false, v5false, v61)) should equal(1.0)
        factor.contains(List(v11, v22, v31, v4false, v5false, v62)) should equal(false)
        factor.get(List(v12, v21, v31, v4false, v5false, v60)) should equal(1.0)
        factor.contains(List(v12, v21, v31, v4false, v5false, v61)) should equal(false)
        factor.contains(List(v12, v21, v31, v4false, v5false, v62)) should equal(false)
        factor.contains(List(v12, v22, v31, v4false, v5false, v60)) should equal(false)
        factor.contains(List(v12, v22, v31, v4false, v5false, v61)) should equal(false)
        factor.get(List(v12, v22, v31, v4false, v5false, v62)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v4false, v5false, v60)) should equal(false)
        factor.get(List(v13, v21, v31, v4false, v5false, v61)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v4false, v5false, v62)) should equal(false)
        factor.get(List(v13, v22, v31, v4false, v5false, v60)) should equal(1.0)
        factor.contains(List(v13, v22, v31, v4false, v5false, v61)) should equal(false)
        factor.contains(List(v13, v22, v31, v4false, v5false, v62)) should equal(false)

        factor.get(List(v11, v21, v31, v4true, v5false, v60)) should equal(1.0)
        factor.contains(List(v11, v21, v31, v4true, v5false, v61)) should equal(false)
        factor.contains(List(v11, v21, v31, v4true, v5false, v62)) should equal(false)
        factor.get(List(v11, v22, v31, v4true, v5false, v60)) should equal(1.0)
        factor.contains(List(v11, v22, v31, v4true, v5false, v61)) should equal(false)
        factor.contains(List(v11, v22, v31, v4true, v5false, v62)) should equal(false)
        factor.get(List(v12, v21, v31, v4true, v5false, v60)) should equal(1.0)
        factor.contains(List(v12, v21, v31, v4true, v5false, v61)) should equal(false)
        factor.contains(List(v12, v21, v31, v4true, v5false, v62)) should equal(false)
        factor.get(List(v12, v22, v31, v4true, v5false, v60)) should equal(1.0)
        factor.contains(List(v12, v22, v31, v4true, v5false, v61)) should equal(false)
        factor.contains(List(v12, v22, v31, v4true, v5false, v62)) should equal(false)
        factor.get(List(v13, v21, v31, v4true, v5false, v60)) should equal(1.0)
        factor.contains(List(v13, v21, v31, v4true, v5false, v61)) should equal(false)
        factor.contains(List(v13, v21, v31, v4true, v5false, v62)) should equal(false)
        factor.get(List(v13, v22, v31, v4true, v5false, v60)) should equal(1.0)
        factor.contains(List(v13, v22, v31, v4true, v5false, v61)) should equal(false)
        factor.contains(List(v13, v22, v31, v4true, v5false, v62)) should equal(false)
        factor.contents.size should equal(12)
      }
    }

    "given an Inject" should {
      "produce a factor that matches its inputs to the correct sequence" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 4, 0.5 -> 5)
        val v3 = Inject(v1, v2)
        Values()(v3)
        Universe.universe.activeElements.foreach(Variable(_))
        val List(factor) = Factory.makeFactorsForElement(v3)

        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v3Vals = Variable(v3).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v24 = v2Vals indexOf Regular(4)
        val v25 = v2Vals indexOf Regular(5)
        val v314 = v3Vals indexOf Regular(List(1, 4))
        val v315 = v3Vals indexOf Regular(List(1, 5))
        val v324 = v3Vals indexOf Regular(List(2, 4))
        val v325 = v3Vals indexOf Regular(List(2, 5))
        val v334 = v3Vals indexOf Regular(List(3, 4))
        val v335 = v3Vals indexOf Regular(List(3, 5))

        factor.get(List(v11, v24, v314)) should equal(1.0)
        factor.get(List(v11, v25, v315)) should equal(1.0)
        factor.get(List(v12, v24, v324)) should equal(1.0)
        factor.get(List(v12, v25, v325)) should equal(1.0)
        factor.get(List(v13, v24, v334)) should equal(1.0)
        factor.get(List(v13, v25, v335)) should equal(1.0)

        factor.get(List(v11, v25, v314)) should equal(0.0)
        factor.get(List(v11, v24, v315)) should equal(0.0)
        factor.get(List(v12, v25, v324)) should equal(0.0)
        factor.get(List(v12, v24, v325)) should equal(0.0)
        factor.get(List(v13, v25, v334)) should equal(0.0)
        factor.get(List(v13, v24, v335)) should equal(0.0)

        factor.get(List(v12, v24, v314)) should equal(0.0)
        factor.get(List(v12, v25, v315)) should equal(0.0)
        factor.get(List(v13, v24, v324)) should equal(0.0)
        factor.get(List(v13, v25, v325)) should equal(0.0)
        factor.get(List(v11, v24, v334)) should equal(0.0)
        factor.get(List(v11, v25, v335)) should equal(0.0)

        factor.get(List(v12, v25, v314)) should equal(0.0)
        factor.get(List(v12, v24, v315)) should equal(0.0)
        factor.get(List(v13, v25, v324)) should equal(0.0)
        factor.get(List(v13, v24, v325)) should equal(0.0)
        factor.get(List(v11, v25, v334)) should equal(0.0)
        factor.get(List(v11, v24, v335)) should equal(0.0)

        factor.get(List(v13, v24, v314)) should equal(0.0)
        factor.get(List(v13, v25, v315)) should equal(0.0)
        factor.get(List(v11, v24, v324)) should equal(0.0)
        factor.get(List(v11, v25, v325)) should equal(0.0)
        factor.get(List(v12, v24, v334)) should equal(0.0)
        factor.get(List(v12, v25, v335)) should equal(0.0)

        factor.get(List(v13, v25, v314)) should equal(0.0)
        factor.get(List(v13, v24, v315)) should equal(0.0)
        factor.get(List(v11, v25, v324)) should equal(0.0)
        factor.get(List(v11, v24, v324)) should equal(0.0)
        factor.get(List(v12, v25, v334)) should equal(0.0)
        factor.get(List(v12, v24, v335)) should equal(0.0)
      }
    }

    "given a non-trivial condition and constraint" should {
      "produce the correct constraint factors" in {
        Universe.createNew()
        val v1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
        v1.setCondition((i: Int) => i != 2)
        v1.setConstraint(((i: Int) => i.toDouble))
        Values()(v1)
        val List(condFactor, constrFactor, _) = Factory.makeFactorsForElement(v1)
        val v1Vals = Variable(v1).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        condFactor.get(List(v11)) should be(1.0 +- 0.000000001)
        condFactor.get(List(v12)) should be(0.0 +- 0.000000001)
        condFactor.get(List(v13)) should be(1.0 +- 0.000000001)
        constrFactor.get(List(v11)) should be(1.0 +- 0.000000001)
        constrFactor.get(List(v12)) should be(2.0 +- 0.000000001)
        constrFactor.get(List(v13)) should be(3.0 +- 0.000000001)
      }
    }

    "given an element whose expanded values are only *" should {
      "produce no factors" in {
        Universe.createNew()
        val f = Flip(0.5)
        val lv = LazyValues()
        lv.expandAll(Set((f, -1)))
        val factors = Factory.makeFactorsForElement(f)
        factors should be(empty)
      }
    }
  }

  "Making a factor for a dependent universe" should {
    "produce a correct dependent factor" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      Values()(x)
      Values()(y)
      val dependentUniverse = new Universe(List(x, y))
      val u1 = Uniform(0.0, 1.0)("", dependentUniverse)
      val u2 = Uniform(0.0, 2.0)("", dependentUniverse)
      val a = CachingChain(x, y, (x: Boolean, y: Int) => if (x || y < 2) u1; else u2)("a", dependentUniverse)
      Values(dependentUniverse)(a)
      val evidence = List(NamedEvidence("a", Condition((d: Double) => d < 0.5)))
      Universe.universe.activeElements.foreach(Variable(_))
      dependentUniverse.activeElements.foreach(Variable(_))
      val factor =
        Factory.makeDependentFactor(Variable.cc, Universe.universe, dependentUniverse, () => ProbEvidenceSampler.computeProbEvidence(20000, evidence)(dependentUniverse))
      val xVar = Variable(x)
      val yVar = Variable(y)
      val variables = factor.variables
      variables.toSet should equal(Set(xVar, yVar))
      val xIndex = variables indexOf xVar
      val yIndex = variables indexOf yVar
      val xFalse = xVar.range indexOf Regular(false)
      val xTrue = xVar.range indexOf Regular(true)
      val y1 = yVar.range indexOf Regular(1)
      val y2 = yVar.range indexOf Regular(2)
      val y3 = yVar.range indexOf Regular(3)
      // If x is true or y is 1, pe is 0.5; if both false, 0.25.
      if (xIndex == 0) {
        factor.get(List(xFalse, y2)) should be(0.25 +- 0.01)
        factor.get(List(xFalse, y3)) should be(0.25 +- 0.01)
        factor.get(List(xFalse, y1)) should be(0.5 +- 0.01)
        factor.get(List(xTrue, y1)) should be(0.5 +- 0.01)
        factor.get(List(xTrue, y2)) should be(0.5 +- 0.01)
        factor.get(List(xTrue, y3)) should be(0.5 +- 0.01)
      } else {
        factor.get(List(y2, xFalse)) should be(0.25 +- 0.01)
        factor.get(List(y3, xFalse)) should be(0.25 +- 0.01)
        factor.get(List(y1, xTrue)) should be(0.5 +- 0.01)
        factor.get(List(y1, xFalse)) should be(0.5 +- 0.01)
        factor.get(List(y2, xFalse)) should be(0.5 +- 0.01)
        factor.get(List(y3, xFalse)) should be(0.5 +- 0.01)
      }
    }
  }

  "Making factors for multiple universes" should {
    "produce the same range of values as if it were in a single universe" when {
      "given a simple model with two universes" in {
        Universe.createNew()
        val v1u1 = Select(0.3 -> 0, 0.5 -> 1, 0.2 -> 3)
        val v2u1 = Apply(v1u1, (i: Int) => i % 3)

        Universe.createNew()
        val v1u2 = Select(0.3 -> 0, 0.5 -> 1, 0.2 -> 3)
        Universe.createNew
        val v2u3 = Apply(v1u1, (i: Int) => i % 3)

        (Variable(v1u1).range) should equal(Variable(v1u2).range)
        (Variable(v2u1).range) should equal(Variable(v2u3).range)
      }

      "given a model with multiple universes" in {
        Universe.createNew()
        val func = (i: Int, b: Boolean) => if (b) i else i + 1
        val v1u1 = Select(0.1 -> 0, 0.2 -> 2, 0.7 -> 5)
        val v2u1 = Flip(0.3)
        val v3u1 = Apply(v1u1, v2u1, func)
        val v4u1 = Flip(0.5)
        val v5u1 = Apply(v3u1, v4u1, func)

        Universe.createNew()
        val v1u2 = Select(0.1 -> 0, 0.2 -> 2, 0.7 -> 5)
        Universe.createNew()
        val v2u3 = Flip(0.3)
        Universe.createNew()
        val v3u4 = Apply(v1u1, v2u1, func)
        Universe.createNew()
        val v4u5 = Flip(0.5)
        Universe.createNew()
        val v5u6 = Apply(v3u1, v4u1, func)

        (Variable(v5u1).range) should equal(Variable(v5u6).range)
      }

      "given a multi-universe model with Chains" in {
        Universe.createNew()
        val func1 = (i: Int) => if (i % 2 == 0) Constant(i) else Select(0.4 -> (i - 1), 0.6 -> (i + 1))
        val func2 = (i: Int) => if (i % 4 == 0) Select(0.2 -> (i - 1), 0.8 -> (i + 1)) else Constant(i)
        val v1u1 = Select(0.2 -> 0, 0.5 -> 3, 0.3 -> 6)
        val v2u1 = Chain(v1u1, func1)
        val v3u1 = Chain(v2u1, func2)

        Universe.createNew()
        val v1u2 = Select(0.2 -> 0, 0.5 -> 3, 0.3 -> 6)
        Universe.createNew()
        val v2u3 = Chain(v1u1, func1)
        Universe.createNew()
        val v3u4 = Chain(v2u1, func2)

        (Variable(v3u1).range) should equal(Variable(v3u4).range)
      }
    }

    "correctly produce a factor between elements in multiple universes" when {
      "given a simple model in two universes" in {
        Universe.createNew()
        val v1 = Select(0.3 -> 0, 0.2 -> 1, 0.4 -> 2, 0.1 -> 3)
        Universe.createNew()
        val v2 = Apply(v1, (i: Int) => i / 2)
        Values()(v2)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v10 = v1Vals indexOf Regular(0)
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v20 = v2Vals indexOf Regular(0)
        val v21 = v2Vals indexOf Regular(1)
        val List(factor) = Factory.makeFactorsForElement(v2)
        factor.get(List(v10, v20)) should equal(1.0)
        factor.contains(List(v10, v21)) should equal(false)
        factor.get(List(v11, v20)) should equal(1.0)
        factor.contains(List(v11, v21)) should equal(false)
        factor.contains(List(v12, v20)) should equal(false)
        factor.get(List(v12, v21)) should equal(1.0)
        factor.contains(List(v13, v20)) should equal(false)
        factor.get(List(v13, v21)) should equal(1.0)
        factor.contents.size should equal(4)
      }

      "given a multi-universe model with Chains" in {
        Universe.createNew()
        val v1 = Select(0.2 -> 0, 0.7 -> 1, 0.1 -> 2)
        Universe.createNew()
        val v2 = Constant(2)
        Universe.createNew()
        val v3 = Select(0.4 -> 0, 0.6 -> 1)
        Universe.createNew()
        val v4 = Chain(v1, (i: Int) => if (i % 2 == 0) v2 else v3)
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v3Vals = Variable(v3).range
        val v4Vals = Variable(v4).range
        val v10 = v1Vals indexOf Regular(0)
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v30 = v3Vals indexOf Regular(0)
        val v31 = v3Vals indexOf Regular(1)
        val v40 = v4Vals indexOf Regular(0)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        Variable(v1)
        Variable(v2)
        Variable(v3)
        Variable(v4)
        val factor = Factory.makeFactorsForElement(v4)
        val List(v4Factor) = combineFactors(factor, SumProductSemiring().asInstanceOf[Semiring[Double]], true)

        v4Factor.get(List(v10, v40, v30, 0)) should equal(0.0)
        v4Factor.get(List(v10, v40, v31, 0)) should equal(0.0)
        v4Factor.get(List(v10, v41, v30, 0)) should equal(0.0)
        v4Factor.get(List(v10, v41, v31, 0)) should equal(0.0)
        v4Factor.get(List(v10, v42, v30, 0)) should equal(1.0)
        v4Factor.get(List(v10, v42, v31, 0)) should equal(1.0)
        v4Factor.get(List(v11, v40, v30, 0)) should equal(1.0)
        v4Factor.get(List(v11, v40, v31, 0)) should equal(0.0)
        v4Factor.get(List(v11, v41, v30, 0)) should equal(0.0)
        v4Factor.get(List(v11, v41, v31, 0)) should equal(1.0)
        v4Factor.get(List(v11, v42, v30, 0)) should equal(0.0)
        v4Factor.get(List(v11, v42, v31, 0)) should equal(0.0)
        v4Factor.get(List(v12, v40, v30, 0)) should equal(0.0)
        v4Factor.get(List(v12, v40, v31, 0)) should equal(0.0)
        v4Factor.get(List(v12, v41, v30, 0)) should equal(0.0)
        v4Factor.get(List(v12, v41, v31, 0)) should equal(0.0)
        v4Factor.get(List(v12, v42, v30, 0)) should equal(1.0)
        v4Factor.get(List(v12, v42, v31, 0)) should equal(1.0)
      }
    }
  }

  val maxElementCount = 6
  val maxSize = 500
  val newFactors = ListBuffer[Factor[Double]]()
  val tempFactors = ListBuffer[Factor[Double]]()

  /**
   * Combines a set of factors into a single larger factor. This method is used when a factor has
   * been decomposed into many dependent Factors and a single Factor is required.
   */
  def combineFactors(oldFactors: List[Factor[Double]], semiring: Semiring[Double], removeTemporaries: Boolean): List[Factor[Double]] = {
    newFactors.clear
    tempFactors.clear

    for (factor <- oldFactors) {
      if (factor.hasStar) {
        newFactors += factor
      } else {
        tempFactors += factor
      }
    }

    var nextFactor = tempFactors.head

    for (factor <- tempFactors.tail) {
      val commonVariables = factor.variables.toSet & nextFactor.variables.toSet

      if (commonVariables.size > 0) {
        val newVariables = factor.variables.toSet -- nextFactor.variables.toSet
        val potentialSize = calculateSize(nextFactor.size, newVariables)
        if ((nextFactor.numVars + newVariables.size) < maxElementCount
          && potentialSize < maxSize) {
          nextFactor = factor.product(nextFactor)
        } else {
          if (removeTemporaries) {
            newFactors ++= reduceFactor(nextFactor, semiring, maxElementCount)
          } else {
            newFactors += nextFactor
          }
          nextFactor = factor
        }
      } else {
        newFactors += nextFactor
        nextFactor = factor
      }
    }

    if (nextFactor.numVars > 0) {
      if (removeTemporaries) {
        newFactors ++= reduceFactor(nextFactor, semiring, maxElementCount)
      } else {
        newFactors += nextFactor
      }
    }
    newFactors.toList
  }

  val variableSet = scala.collection.mutable.Set[Variable[_]]()
  val nextFactors = ListBuffer[Factor[Double]]()

  private def reduceFactor(factor: Factor[Double], semiring: Semiring[Double], maxElementCount: Int): List[Factor[Double]] = {
    variableSet.clear

    var resultFactor = Factory.unit[Double](semiring).product(factor)

    (variableSet /: List(factor))(_ ++= _.variables.asInstanceOf[List[Variable[_]]])
    for (variable <- variableSet.filter { _.isInstanceOf[InternalVariable[_]] }) {
      resultFactor = resultFactor.sumOver(variable)
      variableSet.remove(variable)
    }

    var elementCount = variableSet count (v => !isTemporary(v))

    var tempCount = 0;

    for { variable <- variableSet } {
      if (isTemporary(variable) && elementCount <= maxElementCount) {
        nextFactors.clear
        nextFactors ++= Factory.concreteFactors(Variable.cc, variable.asInstanceOf[ElementVariable[_]].element, false)
        (variableSet /: nextFactors)(_ ++= _.variables.asInstanceOf[List[ElementVariable[_]]])
        elementCount = variableSet count (v => !isTemporary(v))

        for (nextFactor <- nextFactors) {
          resultFactor = resultFactor.product(nextFactor)
        }
        tempCount += 1
      }
    }

    if (tempCount > 0 && elementCount <= maxElementCount) {
      for { variable <- variableSet } {
        if (isTemporary(variable)) {
          resultFactor = resultFactor.sumOver(variable)
        }
      }

    }
    List(resultFactor)
  }

  private def calculateSize(currentSize: Int, variables: Set[Variable[_]]) = {
    (currentSize /: variables)(_ * _.size)
  }
  private def isTemporary[_T](variable: Variable[_]): Boolean = {
    variable match {
      case e: ElementVariable[_] => e.element.isTemporary
      case i: InternalVariable[_] => true
      case _ => false
    }
  }

}
