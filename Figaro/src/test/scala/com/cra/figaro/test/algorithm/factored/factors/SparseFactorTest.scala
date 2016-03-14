/*
 * SparseFactorTest.scala 
 * SparseFactor tests.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 20, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored.factors

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
import com.cra.figaro.algorithm.factored.factors.factory.Factory

class SparseFactorTest extends WordSpec with Matchers with PrivateMethodTester {

  "A sparse factor" when {
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
      val f = new SparseFactor[Double](List(v1, v2, v3), List(v4))
      val indices = List(1, 0, 2, 1)
      f.set(indices, 0.2)
      f.set(indices, 0.3)
      f.get(indices) should equal(0.3)
    }

    "compute the union of variables in two sparse factors and the correct index maps when calling unionVars" in {
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
      val f = new SparseFactor[Double](List(v1, v2, v3), List(v4))
      val g = new SparseFactor[Double](List(v5, v3, v2), List(v6))
      val (parents, output, indexMap1, indexMap2) = f.unionVars(g)
      val union = parents ::: output
      union should equal(List(v1, v2, v3, v4, v5, v6))
      indexMap1 should equal(List(0, 1, 2, 3))
      indexMap2 should equal(List(4, 2, 1, 5))
    }

    "multiplying a factor with a sparse factor" should {
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
        val g = new SparseFactor[Double](List(v4), List(v3))
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
        h.variables should contain (v1)
        h.variables should contain (v2)
        h.variables should contain (v3)
        h.variables should contain (v4)
        h.get(List(0, 0, 0, 0)) should be(0.0 +- 0.0001)
        h.get(List(0, 0, 1, 0)) should be(0.06 +- 0.0001)
        h.get(List(0, 0, 2, 0)) should be(0.12 +- 0.0001)
        h.get(List(0, 1, 0, 0)) should be(0.24 +- 0.0001)
        h.get(List(0, 1, 1, 0)) should be(0.32 +- 0.0001)
        h.get(List(0, 1, 2, 0)) should be(0.4 +- 0.0001)
        h.get(List(1, 0, 0, 0)) should be(0.0 +- 0.0001)
        h.get(List(1, 0, 1, 0)) should be(0.07 +- 0.0001)
        h.get(List(1, 0, 2, 0)) should be(0.14 +- 0.0001)
        h.get(List(1, 1, 0, 0)) should be(0.27 +- 0.0001)
        h.get(List(1, 1, 1, 0)) should be(0.36 +- 0.0001)
        h.get(List(1, 1, 2, 0)) should be(0.45 +- 0.0001)
      }
    }

    "multiplying a sparse factor with a factor" should {
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
        val f = new SparseFactor[Double](List(v1, v2), List(v3))
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
        h.get(List(0, 0, 0, 1)) should be(0.0 +- 0.0001)
        h.get(List(1, 0, 0, 1)) should be(0.07 +- 0.0001)
        h.get(List(2, 0, 0, 1)) should be(0.14 +- 0.0001)
        h.get(List(0, 0, 1, 0)) should be(0.24 +- 0.0001)
        h.get(List(1, 0, 1, 0)) should be(0.32 +- 0.0001)
        h.get(List(2, 0, 1, 0)) should be(0.4 +- 0.0001)
        h.get(List(0, 0, 1, 1)) should be(0.27 +- 0.0001)
        h.get(List(1, 0, 1, 1)) should be(0.36 +- 0.0001)
        h.get(List(2, 0, 1, 1)) should be(0.45 +- 0.0001)
      }
    }

    "multiplying a sparse factor with duplicate variables with a normal factor" should {
      "return the same product when multiplied in either order" in {
        Universe.createNew()

        val f = Flip(0.5)
        val e = f === f

        Values()(f)
        Values()(e)
        val vf = Variable(f)
        val f1 = Factory.makeFactorsForElement(f)
        val f2 = Factory.makeFactorsForElement(e)
        val result21 = f2(0).product(f1(0)/*, SumProductSemiring*/)
        val sum21 = result21.sumOver(vf/*, SumProductSemiring*/)

        val result12 = f1(0).product(f2(0)/*, SumProductSemiring*/)
        val sum12 = result12.sumOver(vf/*, SumProductSemiring*/)

        sum21.get(List(0)) should be(sum12.get(List(0)))
        sum21.get(List(1)) should be(sum12.get(List(1)))
      }
    }

    "multiplying a sparse factor with a normal factor with duplicate variables" should {
      "return the same product when multiplied in either order" in {
        Universe.createNew()

        //        val f = Flip(0.5)
        //        val e = f === f
        val e = new Variable(ValueSet.withoutStar(Set(true, false)))
        val f = new Variable(ValueSet.withoutStar(Set(true, false)))

        val f1 = new SparseFactor[Double](List(), List(f))
        f1.set(List(0), 0.5)
        f1.set(List(1), 0.5)

        val f2 = new BasicFactor[Double](List(f, f), List(e))
        f2.set(List(0, 0, 0), 1.0)
        f2.set(List(0, 0, 1), 0.0)
        f2.set(List(0, 1, 0), 0.0)
        f2.set(List(0, 1, 1), 1.0)
        f2.set(List(1, 0, 0), 0.0)
        f2.set(List(1, 0, 1), 1.0)
        f2.set(List(1, 1, 0), 1.0)
        f2.set(List(1, 1, 1), 0.0)

        val result21 = f2.product(f1/*, SumProductSemiring*/)
        val sum21 = result21.sumOver(f/*, SumProductSemiring*/)

        val result12 = f1.product(f2/*, SumProductSemiring*/)
        val sum12 = result12.sumOver(f/*, SumProductSemiring*/)

        sum21.get(List(0)) should be(sum12.get(List(0)))
        sum21.get(List(1)) should be(sum12.get(List(1)))
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
        val f = new SparseFactor[Double](List(v1, v2), List(v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.sumOver(v3/*, SumProductSemiring*/)
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
        val v1 = Variable(e1)
        val v2 = Variable(e2)
        val v3 = Variable(e3)
        val f = new SparseFactor[Double](List(v1), List(v2))
        f.set(List(0, 0), 0.0)
        f.set(List(1, 0), 0.2)
        f.set(List(2, 0), 0.4)
        val g = f.sumOver(v3/*, SumProductSemiring*/)
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
          val f = new SparseFactor[Double](List(v1, v2), List(v1))
          f.set(List(0, 0, 0), 0.1)
          f.set(List(1, 0, 0), 0.2)
          f.set(List(0, 1, 0), 0.3)
          f.set(List(1, 1, 0), 0.4)
          f.set(List(0, 0, 1), 0.5)
          f.set(List(1, 0, 1), 0.6)
          f.set(List(0, 1, 1), 0.7)
          f.set(List(1, 1, 1), 0.8)
          val g = f.sumOver(v1/*, SumProductSemiring*/)
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
        val f = new SparseFactor[Double](List(v1, v2), List(v3))
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
        val f = new SparseFactor[Double](List(v1, v2), List(v3))
        f.set(List(0, 0, 0), 0.0)
        f.set(List(1, 0, 0), 0.1)
        f.set(List(2, 0, 0), 0.2)
        f.set(List(0, 0, 1), 0.3)
        f.set(List(1, 0, 1), 0.4)
        f.set(List(2, 0, 1), 0.5)
        val g = f.marginalizeTo(SumProductSemiring(), v3)
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
      val f = new SparseFactor[Double](List(v1, v2), List(v3))
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
      val g = f.marginalizeTo(SumProductSemiring(), v1, v3)
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
      val f = new SparseFactor[Double](List(v1, v2), List(v2))
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
    //        val factor = Factory.make(v4)
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
    //        val factor = Factory.make(v4)
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

  }
}
