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
import org.scalatest.{ WordSpec, PrivateMethodTester }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.lazyfactored.{Regular}
import com.cra.figaro.algorithm.lazyfactored.LazyValues


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
      val f = Factory.make[Double](List(v1, v2, v3, v4))
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
      Values()(e1)
      Values()(e2)
      Values()(e3)
      Values()(e4)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = Factory.make[Double](List(v1, v2, v3, v4))
      f.firstIndices should equal(List(0, 0, 0, 0))
    }

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
      val f = Factory.make[Double](List(v1, v2, v3, v4))
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
      Values()(e1)
      Values()(e2)
      Values()(e3)
      Values()(e4)
      val v1 = Variable(e1)
      val v2 = Variable(e2)
      val v3 = Variable(e3)
      val v4 = Variable(e4)
      val f = Factory.make[Double](List(v1, v2, v3, v4))
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
      val f = Factory.make[Double](List(v1, v2, v3, v4))
      val g = Factory.make[Double](List(v5, v3, v2, v6))
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
        Values()(e1)
        Values()(e2)
        Values()(e3)
        Values()(e4)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val v4 = Variable(e4)
        val f = Factory.make[Double](List(v1, v2, v3))
        val g = Factory.make[Double](List(v4, v3))
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
        val h = f.product(g, SumProductSemiring)
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
        val f = Factory.make[Double](List(v1, v2, v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.sumOver(v3, SumProductSemiring)
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
        val f = Factory.make[Double](List(v1, v2))
        f.set(List(0, 0), 0.0)
        f.set(List(1, 0), 0.2)
        f.set(List(2, 0), 0.4)
        val g = f.sumOver(v3, SumProductSemiring)
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
          Values()(e1)
          Values()(e2)
          val v1 = Variable(e1)
          val v2 = Variable(e2)
          val f = Factory.make[Double](List(v1, v2, v1))
          f.set(List(0, 0, 0), 0.1)
          f.set(List(1, 0, 0), 0.2)
          f.set(List(0, 1, 0), 0.3)
          f.set(List(1, 1, 0), 0.4)
          f.set(List(0, 0, 1), 0.5)
          f.set(List(1, 0, 1), 0.6)
          f.set(List(0, 1, 1), 0.7)
          f.set(List(1, 1, 1), 0.8)
          val g = f.sumOver(v1, SumProductSemiring)
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
        val f = Factory.make[Double](List(v1, v2, v3))
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
        Values()(e1)
        Values()(e2)
        Values()(e3)
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = Factory.make[Double](List(v1, v2, v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.marginalizeTo(SumProductSemiring, v3)
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
        val f = Factory.make[Double](List(v1, v2, v3))
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
        val g = f.marginalizeTo(SumProductSemiring, v1, v3)
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
        val f = Factory.make[Double](List(v1, v2, v2))
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
                
        g.variables.size should be (2)
        g.variables.contains(v1) should be (true)
        g.variables.contains(v2) should be (true)
        
        if (g.variables.indexOf(v1) == 0)
        {
        	g.get(List(0, 0)) should be (0.06 +- 0.000001)
        	g.get(List(0, 1)) should be (0.25 +- 0.000001)
        	g.get(List(1, 0)) should be (0.15 +- 0.000001)
        	g.get(List(1, 1)) should be (0.5 +- 0.000001)
        	g.get(List(2, 0)) should be (0.1 +- 0.000001)
        	g.get(List(2, 1)) should be (0.25 +- 0.000001)
        }
        else
        {
            g.get(List(0, 0)) should be (0.06 +- 0.000001)
        	g.get(List(1, 0)) should be (0.25 +- 0.000001)
        	g.get(List(0, 1)) should be (0.15 +- 0.000001)
        	g.get(List(1, 1)) should be (0.5 +- 0.000001)
        	g.get(List(0, 2)) should be (0.1 +- 0.000001)
        	g.get(List(1, 2)) should be (0.25 +- 0.000001)
        }

      }
    }
  "Making factors from an element" when {

         "given a constant" should {
      "produce a single factor with one entry whose value is 1.0" in {
        Universe.createNew()
        val v1 = Constant(7)
        Values()(v1)
        val List(factor) = Factory.make(v1)
        factor.get(List(0)) should equal(1.0)
      }
    }

    "given a simple flip" should {
      "produce a single factor in which the first entry is the probability of true " +
        "and the second entry is the probability of false" in {
          Universe.createNew()
          val v1 = Flip(0.3)
          Values()(v1)
          val List(factor) = Factory.make(v1)
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
          val List(factor) = Factory.make(v2)
          val vals = Variable(v1).range
          println("vals = " + vals)
          println("v2.range = " + Variable(v2).range)
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
        val List(factor) = Factory.make(v1)
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
          val List(factor) = Factory.make(v3)
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
            result(0) = a(0)
            result(v31 + 1) = a(1)
            result(v32 + 1) = a(2)
            result(v33 + 1) = a(3)
            result(v34 + 1) = a(4)
            result(v35 + 1) = a(5)
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
          val selectFactor :: outcomeFactors = Factory.make(v3)
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
          val selectFactor :: outcomeFactors = Factory.make(v5)
          outcomeFactors.size should equal(2)
          val v1Factor = outcomeFactors(v3Index)
          val v2Factor = outcomeFactors(v4Index)
          selectFactor.get(List(0, v102, v204)) should be(0.2 / 0.6 +- 0.0001)
          selectFactor.get(List(1, v102, v204)) should be(0.4 / 0.6 +- 0.0001)
          selectFactor.get(List(0, v102, v206)) should be(0.2 / 0.8 +- 0.0001)
          selectFactor.get(List(1, v102, v206)) should be(0.6 / 0.8 +- 0.0001)
          selectFactor.get(List(0, v108, v204)) should be(0.8 / 1.2 +- 0.0001)
          selectFactor.get(List(1, v108, v204)) should be(0.4 / 1.2 +- 0.0001)
          selectFactor.get(List(0, v108, v206)) should be(0.8 / 1.4 +- 0.0001)
          selectFactor.get(List(1, v108, v206)) should be(0.6 / 1.4 +- 0.0001)
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
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v4Vals = Variable(v4).range
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)

        val factor = Factory.make(v4)
        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
        
        v4Factor.get(List(v1t, v41, v21, 0)) should equal(1.0)
        v4Factor.get(List(v1t, v41, v22, 0)) should equal(0.0)
        v4Factor.get(List(v1t, v42, v21, 0)) should equal(0.0)
        v4Factor.get(List(v1t, v42, v22, 0)) should equal(1.0)
        v4Factor.get(List(v1t, v43, v21, 0)) should equal(0.0)
        v4Factor.get(List(v1t, v43, v22, 0)) should equal(0.0)
        v4Factor.get(List(v1f, v41, v21, 0)) should equal(0.0)
        v4Factor.get(List(v1f, v41, v22, 0)) should equal(0.0)
        v4Factor.get(List(v1f, v42, v21, 0)) should equal(0.0)
        v4Factor.get(List(v1f, v42, v22, 0)) should equal(0.0)
        v4Factor.get(List(v1f, v43, v21, 0)) should equal(1.0)
        v4Factor.get(List(v1f, v43, v22, 0)) should equal(1.0)

      }
      
      "produce a conditional selector for each non-temporary parent value" in {
        Universe.createNew()
        val v1 = Flip(0.2)
        val v4 = Chain(v1, (b: Boolean) => if (b) Select(0.1 -> 1, 0.9 -> 2); else Constant(3))
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v4Vals = Variable(v4).range
        
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)

        val factor = Factory.make(v4)
        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)

        v4Factor.get(List(v1t, v41)) should equal(0.1)
        v4Factor.get(List(v1t, v42)) should equal(0.9)
        v4Factor.get(List(v1t, v43)) should equal(0.0)
        v4Factor.get(List(v1f, v41)) should equal(0.0)
        v4Factor.get(List(v1f, v42)) should equal(0.0)
        v4Factor.get(List(v1f, v43)) should equal(1.0)
      }
    }
    
    "given a CPD with one argument" should {
      "produce a single factor with a case for each parent value" in {
    	  Universe.createNew()
    	  val v1 = Flip(0.2)
    	  
    	  val v2 = CPD(v1, false -> Flip(0.1), true -> Flip(0.7))
    	  Values()(v2)
    	  
    	  val v1Vals = Variable(v1).range
    	  val v2Vals = Variable(v2).range
    	    	  
    	  val v1t = v1Vals indexOf Regular(true)
    	  val v1f = v1Vals indexOf Regular(false)
    	  val v2t = v2Vals indexOf Regular(true)
    	  val v2f = v2Vals indexOf Regular(false)
    	  val v3t = 0
    	  val v3f = 1
    	  val v4t = 0
    	  val v4f = 1
    	  
    	  val factor = Factory.make(v2)
          val List(v2Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
          
    	  v2Factor.get(List(v1t, v2t, v3t, v4t)) should equal(1.0)
    	  v2Factor.get(List(v1t, v2t, v3t, v4f)) should equal(1.0)
    	  v2Factor.get(List(v1t, v2t, v3f, v4t)) should equal(0.0)
    	  v2Factor.get(List(v1t, v2t, v3f, v4f)) should equal(0.0)
    	  v2Factor.get(List(v1t, v2f, v3t, v4t)) should equal(0.0)
    	  v2Factor.get(List(v1t, v2f, v3t, v4f)) should equal(0.0)
    	  v2Factor.get(List(v1t, v2f, v3f, v4t)) should equal(1.0)
    	  v2Factor.get(List(v1t, v2f, v3f, v4f)) should equal(1.0)
    	  v2Factor.get(List(v1f, v2t, v3t, v4t)) should equal(1.0)
    	  v2Factor.get(List(v1f, v2t, v3t, v4f)) should equal(0.0)
    	  v2Factor.get(List(v1f, v2t, v3f, v4t)) should equal(1.0)
    	  v2Factor.get(List(v1f, v2t, v3f, v4f)) should equal(0.0)
    	  v2Factor.get(List(v1f, v2f, v3t, v4t)) should equal(0.0)
    	  v2Factor.get(List(v1f, v2f, v3t, v4f)) should equal(1.0)
    	  v2Factor.get(List(v1f, v2f, v3f, v4t)) should equal(0.0)
    	  v2Factor.get(List(v1f, v2f, v3f, v4f)) should equal(1.0)
      }
    }
    
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
        val List(factor) = Factory.make(v2)
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
        val List(factor) = Factory.make(v3)
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
        val List(factor) = Factory.make(v4)
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
        val List(factor) = Factory.make(v5)
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
        val List(factor) = Factory.make(v6)

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
        Values()(v3)
        val List(factor) = Factory.make(v3)

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
        Values()(v1)
        val List(condFactor, constrFactor, _) = Factory.make(v1)
        val v1Vals = Variable(v1).range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        condFactor.get(List(v11)) should be (1.0 +- 0.000000001)
        condFactor.get(List(v12)) should be (0.0 +- 0.000000001)
        condFactor.get(List(v13)) should be (1.0 +- 0.000000001)
        constrFactor.get(List(v11)) should be (1.0 +- 0.000000001)
        constrFactor.get(List(v12)) should be (2.0 +- 0.000000001)
        constrFactor.get(List(v13)) should be (3.0 +- 0.000000001)
      }
    }
    
    "given an element whose expanded values are only *" should {
      "produce no factors" in {
        Universe.createNew()
        val f = Flip(0.5)
        val lv = LazyValues()
        lv.expandAll(Set((f, -1)))
        val factors = Factory.make(f)
        factors should be (empty)
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
      val factor =
        Factory.makeDependentFactor(Universe.universe, dependentUniverse, () => ProbEvidenceSampler.computeProbEvidence(20000, evidence)(dependentUniverse))
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
        val func = (i: Int, b: Boolean) => if(b) i else i + 1
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
        val func1 = (i: Int) => if(i % 2 == 0) Constant(i) else Select(0.4 -> (i - 1), 0.6 -> (i + 1))
        val func2 = (i: Int) => if(i % 4 == 0) Select(0.2 -> (i - 1), 0.8 -> (i + 1)) else Constant(i)
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
        val List(factor) = Factory.make(v2)
        factor.get(List(v10, v20)) should equal(1.0)
        factor.get(List(v10, v21)) should equal(0.0)
        factor.get(List(v11, v20)) should equal(1.0)
        factor.get(List(v11, v21)) should equal(0.0)
        factor.get(List(v12, v20)) should equal(0.0)
        factor.get(List(v12, v21)) should equal(1.0)
        factor.get(List(v13, v20)) should equal(0.0)
        factor.get(List(v13, v21)) should equal(1.0)
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
        
        val factor = Factory.make(v4)
        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)
        
        v4Factor.get(List(v10, v40, 0, v30)) should equal(0.0)
        v4Factor.get(List(v10, v40, 0, v31)) should equal(0.0)
        v4Factor.get(List(v10, v41, 0, v30)) should equal(0.0)
        v4Factor.get(List(v10, v41, 0, v31)) should equal(0.0)
        v4Factor.get(List(v10, v42, 0, v30)) should equal(1.0)
        v4Factor.get(List(v10, v42, 0, v31)) should equal(1.0)
        v4Factor.get(List(v11, v40, 0, v30)) should equal(1.0)
        v4Factor.get(List(v11, v40, 0, v31)) should equal(0.0)
        v4Factor.get(List(v11, v41, 0, v30)) should equal(0.0)
        v4Factor.get(List(v11, v41, 0, v31)) should equal(1.0)
        v4Factor.get(List(v11, v42, 0, v30)) should equal(0.0)
        v4Factor.get(List(v11, v42, 0, v31)) should equal(0.0)
        v4Factor.get(List(v12, v40, 0, v30)) should equal(0.0)
        v4Factor.get(List(v12, v40, 0, v31)) should equal(0.0)
        v4Factor.get(List(v12, v41, 0, v30)) should equal(0.0)
        v4Factor.get(List(v12, v41, 0, v31)) should equal(0.0)
        v4Factor.get(List(v12, v42, 0, v30)) should equal(1.0)
        v4Factor.get(List(v12, v42, 0, v31)) should equal(1.0)
      }
    }
  }
}
