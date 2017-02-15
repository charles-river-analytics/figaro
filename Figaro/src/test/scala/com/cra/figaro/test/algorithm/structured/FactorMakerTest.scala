/*
 * FactorMakerTest.scala
 * Test of SFI factor creation methods.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured

import org.scalatest.{Matchers, WordSpec}
import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.strategy.solve.ConstantStrategy
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.lazyfactored.{Regular, Star, ValueSet}
import ValueSet.{withStar, withoutStar}
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.library.atomic.discrete.{Binomial, Util}
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.util.MultiSet
import com.cra.figaro.util.HashMultiSet
import com.cra.figaro.library.atomic.discrete.FromRange
import com.cra.figaro.library.collection.MakeArray
import com.cra.figaro.library.compound.FoldLeft
import com.cra.figaro.algorithm.factored.factors.factory.Factory.{makeConditionalSelector, makeTupleVarAndFactor}
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredVE

class FactorMakerTest extends WordSpec with Matchers {
  "Making a tuple variable and factor for a set of variables" should {
    "create a factor whose variables are the targets plus a tuple variable" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val e1 = Flip(0.5)
      val e2 = Constant(1)
      val pr = new Problem(cc)
      pr.add(e1)
      pr.add(e2)
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.generateRange()
      c2.generateRange()
      val v1 = c1.variable
      val v2 = c2.variable
      val (tupleVar, tupleFactor) = makeTupleVarAndFactor(cc, None, v1, v2)

      val vars = tupleFactor.variables
      vars.size should equal (3)
      vars.contains(v1) should equal (true)
      vars.contains(v2) should equal (true)
      vars.contains(tupleVar) should equal (true)
    }

    "create a variable whose range is all the tuples of the extended values of the targets" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val e1 = Flip(0.5)
      val e21 = Constant(1)
      val e22 = Constant(2)
      val e3 = Dist(0.4 -> e21, 0.6 -> e22)
      val pr = new Problem(cc)
      pr.add(e1)
      pr.add(e21)
      pr.add(e3)
      val c1 = cc(e1)
      val c21 = cc(e21)
      val c3 = cc(e3)
      c1.generateRange()
      c21.generateRange()
      c3.generateRange()
      val v1 = c1.variable
      val v3 = c3.variable
      val (tupleVar, tupleFactor) = makeTupleVarAndFactor(cc, None, v1, v3)

      val vs = tupleVar.valueSet
      vs.hasStar should equal (false)
      vs.regularValues.size should equal (4)
      if (tupleFactor.variables(0) == v1) {
        vs.regularValues.contains(List(Regular(true), Regular(1)))
        vs.regularValues.contains(List(Regular(false), Regular(1)))
        vs.regularValues.exists(v => v(0) == Regular(true) && !v(1).isRegular) should equal (true)
        vs.regularValues.exists(v => v(0) == Regular(false) && !v(1).isRegular) should equal (true)
      } else {
        vs.regularValues.contains(List(Regular(1), Regular(true)))
        vs.regularValues.contains(List(Regular(1), Regular(false)))
        vs.regularValues.exists(v => v(1) == Regular(true) && !v(0).isRegular) should equal (true)
        vs.regularValues.exists(v => v(1) == Regular(false) && !v(0).isRegular) should equal (true)
      }
    }

    "create a sparse factor in which extended values are mapped to the corresponding list" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val e1 = Flip(0.5)
      val e21 = Constant(1)
      val e22 = Constant(2)
      val e3 = Dist(0.4 -> e21, 0.6 -> e22)
      val pr = new Problem(cc)
      pr.add(e1)
      pr.add(e21)
      pr.add(e3)
      val c1 = cc(e1)
      val c21 = cc(e21)
      val c3 = cc(e3)
      c1.generateRange()
      c21.generateRange()
      c3.generateRange()
      val v1 = c1.variable
      val v3 = c3.variable
      val (tupleVar, tupleFactor) = makeTupleVarAndFactor(cc, None, v1, v3)

      tupleFactor.contents.size should equal (4)
      val v1IndexT = v1.range.indexOf(Regular(true))
      val v1IndexF = v1.range.indexOf(Regular(false))
      val v3Index1 = v3.range.indexOf(Regular(1))
      val v3IndexStar = v3.range.indexWhere(!_.isRegular)
      if (tupleFactor.variables(0) == v1) {
        val vtIndexT1 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(1))))
        val vtIndexF1 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(1))))
        val vtIndexTStar = tupleVar.range.indexWhere(xv => xv.value(0) == Regular(true) && !xv.value(1).isRegular)
        val vtIndexFStar = tupleVar.range.indexWhere(xv => xv.value(0) == Regular(false) && !xv.value(1).isRegular)
        tupleFactor.get(List(v1IndexT, v3Index1, vtIndexT1)) should equal (1.0)
        tupleFactor.get(List(v1IndexF, v3Index1, vtIndexF1)) should equal (1.0)
        tupleFactor.get(List(v1IndexT, v3IndexStar, vtIndexTStar)) should equal (1.0)
        tupleFactor.get(List(v1IndexF, v3IndexStar, vtIndexFStar)) should equal (1.0)
      } else {
        val vtIndex1T = tupleVar.range.indexOf(Regular(List(Regular(1), Regular(true))))
        val vtIndex1F = tupleVar.range.indexOf(Regular(List(Regular(1), Regular(false))))
        val vtIndexStarT = tupleVar.range.indexWhere(xv => xv.value(1) == Regular(true) && !xv.value(0).isRegular)
        val vtIndexStarF = tupleVar.range.indexWhere(xv => xv.value(1) == Regular(false) && !xv.value(0).isRegular)
        tupleFactor.get(List(v3Index1, v1IndexT, vtIndex1T)) should equal (1.0)
        tupleFactor.get(List(v3Index1, v1IndexF, vtIndex1F)) should equal (1.0)
        tupleFactor.get(List(v3IndexStar, v1IndexT, vtIndexStarT)) should equal (1.0)
        tupleFactor.get(List(v3IndexStar, v1IndexF, vtIndexStarF)) should equal (1.0)
      }
    }
  }

  "Making a conditional selector" should {
    "create a factor whose variables are the pair and the outcome" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val e1 = Flip(0.5)
      val e2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val e3 = Select(0.4 -> 1, 0.6 -> 2)
      val pr = new Problem(cc)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      val v1 = c1.variable
      val v2 = c2.variable
      val v3 = c3.variable
      val (tupleVar, tupleFactor) = makeTupleVarAndFactor(cc, None, v1, v2)
      val selector = makeConditionalSelector(tupleVar, Regular(true), v3, Set())

      selector.variables should equal (List(tupleVar, v3))
    }

    "without *, create a factor with the correct cares and don't cares" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val e1 = Flip(0.5)
      val e2 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
      val e3 = Select(0.4 -> 1, 0.6 -> 2)
      val pr = new Problem(cc)
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      val v1 = c1.variable
      val v2 = c2.variable
      val v3 = c3.variable
      val (tupleVar, tupleFactor) = makeTupleVarAndFactor(cc, None, v1, v2)
      val selector = makeConditionalSelector(tupleVar, Regular(true), v3, Set())

      val ctIndexT2 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(2))))
      val ctIndexF2 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(2))))
      val ctIndexT3 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(3))))
      val ctIndexF3 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(3))))
      val c3Index2 = v3.range.indexOf(Regular(2))
      selector.get(List(ctIndexT2, c3Index2)) should equal (1.0)
      selector.get(List(ctIndexT3, c3Index2)) should equal (0.0)
      selector.get(List(ctIndexF2, c3Index2)) should equal (1.0)
      selector.get(List(ctIndexF3, c3Index2)) should equal (1.0)

    }

    "with *, create a factor with the correct cares and don't cares" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val e0t = Constant(true)
      val e0f = Constant(false)
      val e1 = Dist(0.5 -> e0t, 0.5 -> e0f)
      val e41 = Constant(1)
      val e42 = Constant(2)
      val e43 = Constant(3)
      val e2 = Dist(0.2 -> e41, 0.3 -> e42, 0.5 -> e43)
      val e3 = Dist(0.4 -> e41, 0.6 -> e42)
      val pr = new Problem(cc)
      pr.add(e0f)
      pr.add(e1)
      pr.add(e2)
      pr.add(e42)
      pr.add(e43)
      pr.add(e3)
      val c0f = cc(e0f)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c42 = cc(e42)
      val c43 = cc(e43)
      val c3 = cc(e3)
      c0f.generateRange()
      c1.generateRange()
      c42.generateRange()
      c43.generateRange()
      c2.generateRange()
      c3.generateRange()
      val v1 = c1.variable
      val v2 = c2.variable
      val v3 = c3.variable
      val (tupleVar, tupleFactor) = makeTupleVarAndFactor(cc, None, v1, v2)

      val c1IndexStar = v1.range.indexWhere(!_.isRegular)
      val c1Star = v1.range(c1IndexStar)
      val c3IndexStar = v1.range.indexWhere(!_.isRegular)
      val c3Star = v3.range(c3IndexStar)
      val selectorF = makeConditionalSelector(tupleVar, Regular(false), v3, Set())
      val selectorStar = makeConditionalSelector(tupleVar, c1Star, v3, Set())
      val ctIndexStar2 = tupleVar.range.indexOf(Regular(List(c1Star, Regular(2))))
      val ctIndexF2 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(2))))
      val ctIndexStar3 = tupleVar.range.indexOf(Regular(List(c1Star, Regular(3))))
      val ctIndexStarStar = tupleVar.range.indexOf(Regular(List(c1Star, c3Star)))
      val ctIndexF3 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(3))))
      val c3Index2 = v3.range.indexOf(Regular(2))
      selectorF.get(List(ctIndexF2, c3Index2)) should equal (1.0)
      selectorF.get(List(ctIndexF3, c3Index2)) should equal (0.0)
      selectorF.get(List(ctIndexF2, c3IndexStar)) should equal (0.0)
      selectorF.get(List(ctIndexStar2, c3Index2)) should equal (1.0)
      selectorF.get(List(ctIndexStar3, c3Index2)) should equal (1.0)
      selectorF.get(List(ctIndexStar2, c3IndexStar)) should equal (1.0)
      selectorStar.get(List(ctIndexF2, c3Index2)) should equal (1.0)
      selectorStar.get(List(ctIndexF3, c3Index2)) should equal (1.0)
      selectorStar.get(List(ctIndexF2, c3IndexStar)) should equal (1.0)
      // When the parent value is *, the only possible value for the outcome is *
      selectorStar.get(List(ctIndexStar2, c3Index2)) should equal (0.0)
      selectorStar.get(List(ctIndexStar3, c3Index2)) should equal (0.0)
      selectorStar.get(List(ctIndexStar2, c3IndexStar)) should equal (0.0)
      selectorStar.get(List(ctIndexStarStar, c3IndexStar)) should equal (1.0)

    }
  }

  "Making factors from an element" when {

    "given a constant" should {
      "produce a single factor with one entry whose value is 1.0" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(7)
        pr.add(v1)
        val c1 = cc(v1)
        c1.generateRange()
        c1.makeNonConstraintFactors()

        val List(factor) = c1.nonConstraintFactors
        factor.variables should equal (List(c1.variable))
        factor.size should equal (1)
        factor.get(List(0)) should equal(1.0)
      }
    }

    "given an atomic flip" should {
      "produce a single factor in which the first entry is the probability of true " +
        "and the second entry is the probability of false" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Flip(0.3)
          pr.add(v1)
          val c1 = cc(v1)
          c1.generateRange()
          c1.makeNonConstraintFactors()

          val List(factor) = c1.nonConstraintFactors
          factor.variables should equal (List(c1.variable))
          factor.size should equal (2)
          factor.get(List(0)) should equal(0.3)
          factor.get(List(1)) should equal(0.7)
        }
    }

    "given a compound flip with an added parent without *" should {
      "produce a single factor in which each possible value of the parent is associated with two " +
        "entries, one each for true and false, with the appropriate probabilities" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Select(0.2 -> 0.1, 0.8 -> 0.3)
          val v2 = Flip(v1)
          pr.add(v1)
          pr.add(v2)
          val c1 = cc(v1)
          val c2 = cc(v2)
          c1.generateRange()
          c2.generateRange()
          c2.makeNonConstraintFactors()

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c1.variable, c2.variable))
          factor.size should equal (4)
          val vals = c1.variable.range
          val i1 = vals.indexOf(Regular(0.1))
          val i2 = vals.toList.indexOf(Regular(0.3))
          factor.get(List(i1, 0)) should equal(0.1)
          factor.get(List(i1, 1)) should equal(0.9)
          factor.get(List(i2, 0)) should equal(0.3)
          factor.get(List(i2, 1)) should equal(0.7)
        }
    }

    "given a compound flip with an added parent with *" should {
      "produce the flip factor with regular values having positive probability when the parent is regular and * having probability 1.0 when the parent is *" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Constant(0.1)
          val v2 = Constant(0.3)
          val v3 = Dist(0.2 -> v1, 0.8 -> v2)
          val v4 = Flip(v3)
          pr.add(v2)
          pr.add(v3)
          pr.add(v4)
          val c2 = cc(v2)
          val c3 = cc(v3)
          val c4 = cc(v4)
          c2.generateRange()
          c3.generateRange()
          c4.generateRange()
          c4.makeNonConstraintFactors()

          val List(factor) = c4.nonConstraintFactors
          factor.variables should equal (List(c3.variable, c4.variable))
          factor.size should equal (6) // the parent has two values: * and e3; the flip has three values: true, false, and *
          val parentStarIndex = c3.variable.range.indexWhere(!_.isRegular)
          val parentRegularIndex = 1 - parentStarIndex
          val flipStarIndex = c4.variable.range.indexWhere(!_.isRegular)
          val flipTrueIndex = c4.variable.range.indexOf(Regular(true))
          val flipFalseIndex = c4.variable.range.indexOf(Regular(false))
          factor.get(List(parentStarIndex, flipStarIndex)) should equal (1.0)
          factor.get(List(parentStarIndex, flipTrueIndex)) should equal (0.0)
          factor.get(List(parentStarIndex, flipFalseIndex)) should equal (0.0)
          factor.get(List(parentRegularIndex, flipStarIndex)) should equal (0.0)
          factor.get(List(parentRegularIndex, flipTrueIndex)) should equal (0.3)
          factor.get(List(parentRegularIndex, flipFalseIndex)) should equal (0.7)
      }
    }

    "given a compound flip with an unadded parent" should {
      "produce the factor where only * has positive probability and not add the parent" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Select(0.2 -> 0.1, 0.8 -> 0.3)
          val v2 = Flip(v1)
          pr.add(v2)
          val c2 = cc(v2)
          c2.generateRange()
          c2.makeNonConstraintFactors()

          val List(factor) = c2.nonConstraintFactors
          factor.variables.size should equal (2)
          factor.variables(1) should equal (c2.variable)
          factor.size should equal (3)
          val flipStarIndex = c2.variable.range.indexWhere(!_.isRegular)
          val flipTrueIndex = c2.variable.range.indexOf(Regular(true))
          val flipFalseIndex = c2.variable.range.indexOf(Regular(false))
          factor.get(List(0, flipStarIndex)) should equal (1.0)
          factor.get(List(0, flipTrueIndex)) should equal (0.0)
          factor.get(List(0, flipFalseIndex)) should equal (0.0)
          cc.contains(v1) should equal (false)
      }
    }

    "given a parameterized flip, whether or not the parent has been added, with the parameterized flag = true" should {
      "produce the factor where the probability of true is equal to the MAP value of the parameter" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Beta(2,2)
        val v2 = Flip(v1)
        pr.add(v1)
        pr.add(v2)
        val c1 = cc(v1)
        val c2 = cc(v2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors(true)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c2.variable))
        factor.size should equal (2)
        val trueIndex = c2.variable.range.indexOf(Regular(true))
        factor.get(List(trueIndex)) should equal (v1.MAPValue)
        factor.get(List(1 - trueIndex)) should equal (1 - v1.MAPValue)
      }
    }

    "given a parameterized flip with an unadded parent, with the parameterized flag = true" should {
      "produce the factor where the probability of true is equal to the MAP value of the parameter, and * has probability 0" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Beta(2,2)
        val v2 = Flip(v1)
        pr.add(v2)
        val c2 = cc(v2)
        c2.generateRange()
        c2.makeNonConstraintFactors(true)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c2.variable))
        factor.size should equal (3)
        val trueIndex = c2.variable.range.indexOf(Regular(true))
        val falseIndex = c2.variable.range.indexOf(Regular(false))
        val starIndex = c2.variable.range.indexWhere(!_.isRegular)
        factor.get(List(trueIndex)) should equal (v1.MAPValue)
        factor.get(List(falseIndex)) should equal (1 - v1.MAPValue)
        factor.get(List(starIndex)) should equal (0.0)
      }
    }

    "given a parameterized flip with an added parent, with the parameterized flag = false" should {
      "treat like a compound flip" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Beta(2,2)
        val v2 = Flip(v1)
        pr.add(v1)
        pr.add(v2)
        val c1 = cc(v1)
        val c2 = cc(v2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors(false)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c1.variable, c2.variable))
        factor.size should equal (ParticleGenerator.defaultMaxNumSamplesAtChain * 2)
        for { (p, index) <- c1.variable.range.zipWithIndex } {
          factor.get(List(index, 0)) should equal (p.value)
          factor.get(List(index, 1)) should equal (1 - p.value)
        }
      }
    }

    "given a parameterized flip with an unadded parent, with the parameterized flag = false" should {
      "treat like a compound flip" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Beta(2,2)
        val v2 = Flip(v1)
        pr.add(v2)
        val c2 = cc(v2)
        c2.generateRange()
        c2.makeNonConstraintFactors(false)

        val List(factor) = c2.nonConstraintFactors
        factor.variables.size should equal (2)
        factor.variables(1) should equal (c2.variable)
        factor.size should equal (3)
        val flipStarIndex = c2.variable.range.indexWhere(!_.isRegular)
        val flipTrueIndex = c2.variable.range.indexOf(Regular(true))
        val flipFalseIndex = c2.variable.range.indexOf(Regular(false))
        factor.get(List(0, flipStarIndex)) should equal (1.0)
        factor.get(List(0, flipTrueIndex)) should equal (0.0)
        factor.get(List(0, flipFalseIndex)) should equal (0.0)
        cc.contains(v1) should equal (false)
      }
    }

    "given an atomic select" should {
      "produce a single factor in which each possible value is associated with the correct probability" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.2 -> 1, 0.3 -> 0, 0.1 -> 2, 0.05 -> 5, 0.35 -> 4)
        pr.add(v1)
        val c1 = cc(v1)
        c1.generateRange()
        c1.makeNonConstraintFactors()

        val List(factor) = c1.nonConstraintFactors
        val vals = c1.variable.range
        val i1 = vals.indexOf(Regular(1))
        val i0 = vals.indexOf(Regular(0))
        val i2 = vals.indexOf(Regular(2))
        val i5 = vals.indexOf(Regular(5))
        val i4 = vals.indexOf(Regular(4))
        factor.get(List(i1)) should equal(0.2)
        factor.get(List(i0)) should equal(0.3)
        factor.get(List(i2)) should equal(0.1)
        factor.get(List(i5)) should equal(0.05)
        factor.get(List(i4)) should equal(0.35)
      }
    }

    "given a compound select with an added parent without *" should {
      "produce a single factor over all the probability variables and the select variable in which each value has the correct probability" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Select(0.5 -> 0.1, 0.5 -> 0.3)
        val e2 = Constant(0.8)
        val e3 = Constant(0.9)
        val e4 = Select(e1 -> 1, e2 -> 2, e3 -> 3)
        pr.add(e1)
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val List(factor) = c4.nonConstraintFactors
        factor.variables should equal (List(c1.variable, c2.variable, c3.variable, c4.variable))
        factor.size should equal (6)
        val norm1 = 0.1 + 0.8 + 0.9
        val norm2 = 0.3 + 0.8 + 0.9
        factor.get(List(0, 0, 0, 0)) should be ((0.1 / norm1) +- 0.000000001)
        factor.get(List(0, 0, 0, 1)) should be ((0.8 / norm1) +- 0.000000001)
        factor.get(List(0, 0, 0, 2)) should be ((0.9 / norm1) +- 0.000000001)
        factor.get(List(1, 0, 0, 0)) should be ((0.3 / norm2) +- 0.000000001)
        factor.get(List(1, 0, 0, 1)) should be ((0.8 / norm2) +- 0.000000001)
        factor.get(List(1, 0, 0, 2)) should be ((0.9 / norm2) +- 0.000000001)
      }
    }

    "given a compound select with an added parent with *" should {
      "produce a single factor over all the probability variables and the select variable in which each value has the correct probability" +
      "when the parents are regular and * has probability 1 when a parent is *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Constant(0.1)
        val e2 = Constant(0.3)
        val e3 = Select(e1 -> 0.1, e2 -> 0.3)
        val e4 = Constant(0.8)
        val e5 = Constant(0.9)
        val e6 = Select(e3 -> 1, e4 -> 2, e5 -> 3)
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        pr.add(e5)
        pr.add(e6)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        val c5 = cc(e5)
        val c6 = cc(e6)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c6.makeNonConstraintFactors()

        val List(factor) = c6.nonConstraintFactors
        factor.variables should equal (List(c3.variable, c4.variable, c5.variable, c6.variable))
        factor.size should equal (12)
        val c3StarIndex = c3.variable.range.indexWhere(!_.isRegular)
        val c301Index = c3.variable.range.indexOf(Regular(0.1))
        val c303Index = c3.variable.range.indexOf(Regular(0.3))
        val c6StarIndex = c6.variable.range.indexWhere (!_.isRegular)
        val c61Index = c6.variable.range.indexOf(Regular(1))
        val c62Index = c6.variable.range.indexOf(Regular(2))
        val c63Index = c6.variable.range.indexOf(Regular(3))
        val norm1 = 0.1 + 0.8 + 0.9
        val norm2 = 0.3 + 0.8 + 0.9
        factor.get(List(c3StarIndex, 0, 0, c6StarIndex)) should equal (1.0)
        factor.get(List(c3StarIndex, 0, 0, c61Index)) should equal (0.0)
        factor.get(List(c3StarIndex, 0, 0, c62Index)) should equal (0.0)
        factor.get(List(c3StarIndex, 0, 0, c63Index)) should equal (0.0)
        factor.get(List(c301Index, 0, 0, c6StarIndex)) should equal (0.0)
        factor.get(List(c301Index, 0, 0, c61Index)) should be ((0.1 / norm1) +- 0.000000001)
        factor.get(List(c301Index, 0, 0, c62Index)) should be ((0.8 / norm1) +- 0.000000001)
        factor.get(List(c301Index, 0, 0, c63Index)) should be ((0.9 / norm1) +- 0.000000001)
        factor.get(List(c303Index, 0, 0, c6StarIndex)) should equal (0.0)
        factor.get(List(c303Index, 0, 0, c61Index)) should be ((0.3 / norm2) +- 0.000000001)
        factor.get(List(c303Index, 0, 0, c62Index)) should be ((0.8 / norm2) +- 0.000000001)
        factor.get(List(c303Index, 0, 0, c63Index)) should be ((0.9 / norm2) +- 0.000000001)
      }
    }

    "given a compound select with an unadded parent" should {
      "produce the factor where only * has positive probability and not add the parent" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Constant(0.6)
          val v2 = Select(0.2 -> 0.1, 0.8 -> 0.3)
          val v3 = Select(v1 -> 1, v2 -> 2)
          pr.add(v2)
          pr.add(v3)
          val c2 = cc(v2)
          val c3 = cc(v3)
          c2.generateRange()
          c3.generateRange()
          c3.makeNonConstraintFactors()
          val List(factor) = c3.nonConstraintFactors

          factor.variables.size should equal (3)
          factor.variables(1) should equal (c2.variable)
          factor.variables(2) should equal (c3.variable)
          factor.size should equal (6)
          val c3StarIndex = c3.variable.range.indexWhere(!_.isRegular)
          val c31Index = c3.variable.range.indexOf(Regular(1))
          val c32Index = c3.variable.range.indexOf(Regular(2))
          val c201Index = c2.variable.range.indexOf(Regular(0.1))
          val c203Index = c2.variable.range.indexOf(Regular(0.3))
          factor.get(List(0, c201Index, c3StarIndex)) should equal (1.0)
          factor.get(List(0, c201Index, c31Index)) should equal (0.0)
          factor.get(List(0, c201Index, c32Index)) should equal (0.0)
          factor.get(List(0, c203Index, c3StarIndex)) should equal (1.0)
          factor.get(List(0, c203Index, c31Index)) should equal (0.0)
          factor.get(List(0, c203Index, c32Index)) should equal (0.0)
          cc.contains(v1) should equal (false)
      }
    }

    "given a parameterized select with an added parent, with the parameterized flag = true" should {
      "produce the factor where the probability of the outcomes is equal to the MAP value of the parameter" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Dirichlet(2,2)
        val v2 = Select(v1, false, true)
        pr.add(v1)
        pr.add(v2)
        val c1 = cc(v1)
        val c2 = cc(v2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors(true)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c2.variable))
        factor.size should equal (2)
        val pFalse = v1.MAPValue(0)
        val pTrue = v1.MAPValue(1)
        val trueIndex = c2.variable.range.indexOf(Regular(true))
        factor.get(List(trueIndex)) should equal (pTrue)
        factor.get(List(1 - trueIndex)) should equal (pFalse)
      }
    }

    "given a parameterized select with an unadded parent, with the parameterized flag = true" should {
      "produce the factor where the probability of the outcomes is equal to the MAP value of the parameter" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Dirichlet(2,2)
        val v2 = Select(v1, false, true)
        pr.add(v2)
        val c2 = cc(v2)
        c2.generateRange()
        c2.makeNonConstraintFactors(true)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c2.variable))
        factor.size should equal (3)
        val pFalse = v1.MAPValue(0)
        val pTrue = v1.MAPValue(1)
        val trueIndex = c2.variable.range.indexOf(Regular(true))
        val falseIndex = c2.variable.range.indexOf(Regular(false))
        val starIndex = c2.variable.range.indexWhere(!_.isRegular)
        factor.get(List(trueIndex)) should equal (pTrue)
        factor.get(List(falseIndex)) should equal (pFalse)
        factor.get(List(starIndex)) should equal (0.0)
      }
    }

    "given a parameterized select with an added parent, with the parameterized flag = false" should {
      "create a factor over the parameter and the select in which the probability of a value of a select is equal to the " +
      "parameter value at that value" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Dirichlet(2,2)
        val v2 = Select(v1, false, true)
        pr.add(v1)
        pr.add(v2)
        val c1 = cc(v1)
        val c2 = cc(v2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors(false)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c1.variable, c2.variable))
        factor.size should equal (ParticleGenerator.defaultMaxNumSamplesAtChain * 2)
        for {
          (xprobs, i) <- c1.variable.range.zipWithIndex
          j <- 0 until c2.variable.range.size
        } {
          factor.get(List(i, j)) should equal (xprobs.value(j))
        }
      }
    }

    "given a parameterized select with an unadded parent, with the parameterized flag = false" should {
      "return a factor over the select variable that assigns probability 1 to * and probability 0 to true and false" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Dirichlet(2,2)
        val v2 = Select(v1, false, true)
        pr.add(v2)
        val c2 = cc(v2)
        c2.generateRange()
        c2.makeNonConstraintFactors(false)

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c2.variable))
        factor.size should equal (3)
        val trueIndex = c2.variable.range.indexOf(Regular(true))
        val falseIndex = c2.variable.range.indexOf(Regular(false))
        val starIndex = c2.variable.range.indexWhere(!_.isRegular)
        factor.get(List(trueIndex)) should equal (0.0)
        factor.get(List(falseIndex)) should equal (0.0)
        factor.get(List(starIndex)) should equal (1.0)
      }
    }

    "given an atomic binomial" should {
      "produce a factor with the correct probability for every outcome" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Binomial(3, 0.6)
          pr.add(v1)
          val c1 = cc(v1)
          c1.generateRange()
          c1.makeNonConstraintFactors()

          val List(factor) = c1.nonConstraintFactors
          factor.variables should equal (List(c1.variable))
          factor.size should equal (4)
          factor.get(List(0)) should equal (Util.binomialDensity(3, 0.6, 0))
          factor.get(List(1)) should equal (Util.binomialDensity(3, 0.6, 1))
          factor.get(List(2)) should equal (Util.binomialDensity(3, 0.6, 2))
          factor.get(List(3)) should equal (Util.binomialDensity(3, 0.6, 3))
      }
    }

    "given a parameterized binomial with a fixed number of trials with an added success probability, when the parameterized flag = true" should {
      "produce a factor where the probability of every outcome is the correct probability given the MAP success probability" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Beta(2, 3)
          val v2 = Binomial(3, v1)
          pr.add(v1)
          pr.add(v2)
          val c1 = cc(v1)
          val c2 = cc(v2)
          c1.generateRange()
          c2.generateRange()
          c2.makeNonConstraintFactors(true)

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c2.variable))
          factor.size should equal (4)
          val probSuccess = v1.MAPValue
          for { n <- 0 to 3 } {
            val index = c2.variable.range.indexOf(Regular(n))
            factor.get(List(index)) should equal (Util.binomialDensity(3, probSuccess, n))
          }
      }
    }

    "given a parameterized binomial with a fixed number of trials with an unadded success probability, when the parameterized flag = true" should {
      "produce a factor that assigns probability 1 to * and 0 to every outcome" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Beta(2, 3)
          val v2 = Binomial(3, v1)
          pr.add(v2)
          val c2 = cc(v2)
          c2.generateRange()
          c2.makeNonConstraintFactors(true)

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c2.variable))
          factor.size should equal (5)
          val probSuccess = v1.MAPValue
          factor.get(List(c2.variable.range.indexWhere(!_.isRegular))) should equal (1.0)
          for { n <- 0 to 3 } {
            val index = c2.variable.range.indexOf(Regular(n))
            factor.get(List(index)) should equal (0.0)
          }
        }
      }


  "given a parameterized binomial with a fixed number of trials with an added success probability, when the parameterized flag = false" should {
      "use the chain decomposition, conditioning on the values of the parameter" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Beta(2, 3)
          val v2 = Binomial(3, v1)
          pr.add(v1)
          pr.add(v2)
          val c1 = cc(v1)
          val c2 = cc(v2)
          c1.generateRange()
          c2.expand() // need to do this so the atomic binomials for each of the beta values is added to the problem
          c2.generateRange()
          c2.makeNonConstraintFactors(false)

          val tupleFactor :: factors = c2.nonConstraintFactors
          val List(var1, var2, tupleVar) = tupleFactor.variables
          var1 should equal (c1.variable)
          var2 should equal (c2.variable)
          factors.size should equal (ParticleGenerator.defaultMaxNumSamplesAtChain)
          val vars = factors(0).variables
          vars.size should equal (2)
          vars(0) should equal (tupleVar)
      }
    }

  "given a parameterized binomial with a fixed number of trials with an unadded success probability, when the parameterized flag = false" should {
      "create a simple factor with probability 1 for * and probability 0 for the outcomes" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Beta(2, 3)
          val v2 = Binomial(3, v1)
          pr.add(v2)
          val c2 = cc(v2)
          c2.generateRange()
          c2.makeNonConstraintFactors(false)

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c2.variable))
          factor.size should equal (5)
          val probSuccess = v1.MAPValue
          factor.get(List(c2.variable.range.indexWhere(!_.isRegular))) should equal (1.0)
          for { n <- 0 to 3 } {
            val index = c2.variable.range.indexOf(Regular(n))
            factor.get(List(index)) should equal (0.0)
          }
      }
    }

    "given an atomic without an explicit factor maker" should {
      "automatically sample the element" in {
        val universe = Universe.createNew()
        val pg = ParticleGenerator(universe)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Normal(0.0, 1.0)
        pr.add(v1)
        val c1 = cc(v1)
        c1.generateRange()
        c1.makeNonConstraintFactors()

        val List(factor) = c1.nonConstraintFactors
        factor.variables should equal (List(c1.variable))
        factor.size should equal (ParticleGenerator.defaultMaxNumSamplesAtChain)
      }
    }

    "given an atomic dist" should {
      "produce a list of factors, one representing the choice over outcomes, a tuple factor, and a conditional selector for each outcome" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Flip(0.2)
          val v2 = Constant(false)
          val v3 = Dist(0.3 -> v1, 0.7 -> v2)
          pr.add(v1)
          pr.add(v2)
          pr.add(v3)
          val c1 = cc(v1)
          val c2 = cc(v2)
          val c3 = cc(v3)
          c1.generateRange()
          c2.generateRange()
          c3.generateRange()
          c3.makeNonConstraintFactors()

          val v1Vals = c1.variable.range
          val v3Vals = c3.variable.range
          val v1IndexT = v1Vals.indexOf(Regular(true))
          val v1IndexF = v1Vals.indexOf(Regular(false))
          val v3IndexT = v3Vals.indexOf(Regular(true))
          val v3IndexF = v3Vals.indexOf(Regular(false))
          val v1Index = v3.outcomes.indexOf(v1)
          val v2Index = v3.outcomes.indexOf(v2)
          val selectFactor :: tupleFactor :: outcomeFactors = c3.nonConstraintFactors
          tupleFactor.variables.size should equal (3)
          val selectVar = tupleFactor.variables(0)
          tupleFactor.variables(1) should equal (c3.variable)
          val tupleVar = tupleFactor.variables(2)
          outcomeFactors.size should equal(2)
          outcomeFactors(0).variables(0) should equal (tupleVar)
          outcomeFactors(1).variables(0) should equal (tupleVar)
          val vtIndex0T = tupleVar.range.indexOf(Regular(List(Regular(0), Regular(true))))
          val vtIndex0F = tupleVar.range.indexOf(Regular(List(Regular(0), Regular(false))))
          val vtIndex1T = tupleVar.range.indexOf(Regular(List(Regular(1), Regular(true))))
          val vtIndex1F = tupleVar.range.indexOf(Regular(List(Regular(1), Regular(false))))
          val v1Factor = outcomeFactors(v1Index)
          val v2Factor = outcomeFactors(v2Index)
          selectFactor.get(List(v1Index)) should equal(0.3)
          selectFactor.get(List(v2Index)) should equal(0.7)
          v1Factor.get(List(vtIndex0T, v3IndexT)) should equal(1.0)
          v1Factor.get(List(vtIndex0T, v3IndexF)) should equal(0.0)
          v1Factor.get(List(vtIndex0F, v3IndexT)) should equal(0.0)
          v1Factor.get(List(vtIndex0F, v3IndexF)) should equal(1.0)
          v1Factor.get(List(vtIndex1T, v3IndexT)) should equal(1.0)
          v1Factor.get(List(vtIndex1T, v3IndexF)) should equal(1.0)
          v1Factor.get(List(vtIndex1F, v3IndexT)) should equal(1.0)
          v1Factor.get(List(vtIndex1F, v3IndexF)) should equal(1.0)
      }
    }

    "given a compound dist" should {
      "produce a list of factors, one representing the choice over outcomes, a tuple factor, and a conditional selector for each outcome" in {
          Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val v1 = Select(0.2 -> 0.2, 0.8 -> 0.8)
          val v2 = Select(0.4 -> 0.4, 0.6 -> 0.6)
          val v3 = Flip(0.2)
          val v4 = Constant(false)
          val v5 = Dist(v1 -> v3, v2 -> v4)
          pr.add(v1)
          pr.add(v2)
          pr.add(v3)
          pr.add(v4)
          pr.add(v5)
          val c1 = cc(v1)
          val c2 = cc(v2)
          val c3 = cc(v3)
          val c4 = cc(v4)
          val c5 = cc(v5)
          c1.generateRange()
          c2.generateRange()
          c3.generateRange()
          c4.generateRange()
          c5.generateRange()
          c5.makeNonConstraintFactors()

          val v1Vals = c1.variable.range
          val v2Vals = c2.variable.range
          val v3Vals = c3.variable.range
          val v4Vals = c4.variable.range
          val v5Vals = c5.variable.range
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
          val selectFactor :: tupleFactor :: outcomeFactors = c5.nonConstraintFactors
          tupleFactor.variables.size should equal (3)
          val selectVar = tupleFactor.variables(0)
          tupleFactor.variables(1) should equal (c5.variable)
          val tupleVar = tupleFactor.variables(2)
          outcomeFactors.size should equal(2)
          outcomeFactors(0).variables(0) should equal (tupleVar)
          outcomeFactors(1).variables(0) should equal (tupleVar)
          val vtIndex0T = tupleVar.range.indexOf(Regular(List(Regular(0), Regular(true))))
          val vtIndex0F = tupleVar.range.indexOf(Regular(List(Regular(0), Regular(false))))
          val vtIndex1T = tupleVar.range.indexOf(Regular(List(Regular(1), Regular(true))))
          val vtIndex1F = tupleVar.range.indexOf(Regular(List(Regular(1), Regular(false))))
          val v1Factor = outcomeFactors(v3Index)
          val v2Factor = outcomeFactors(v4Index)
          selectFactor.variables(0) should equal (c1.variable)
          selectFactor.variables(1) should equal (c2.variable)
          selectFactor.variables(2) should equal (selectVar)
          selectFactor.get(List(v102, v204, 0)) should be(0.2 / 0.6 +- 0.0001)
          selectFactor.get(List(v102, v204, 1)) should be(0.4 / 0.6 +- 0.0001)
          selectFactor.get(List(v102, v206, 0)) should be(0.2 / 0.8 +- 0.0001)
          selectFactor.get(List(v102, v206, 1)) should be(0.6 / 0.8 +- 0.0001)
          selectFactor.get(List(v108, v204, 0)) should be(0.8 / 1.2 +- 0.0001)
          selectFactor.get(List(v108, v204, 1)) should be(0.4 / 1.2 +- 0.0001)
          selectFactor.get(List(v108, v206, 0)) should be(0.8 / 1.4 +- 0.0001)
          selectFactor.get(List(v108, v206, 1)) should be(0.6 / 1.4 +- 0.0001)
          v1Factor.get(List(vtIndex0T, v5t)) should equal(1.0)
          v1Factor.get(List(vtIndex0F, v5t)) should equal(0.0)
          v1Factor.get(List(vtIndex0T, v5f)) should equal(0.0)
          v1Factor.get(List(vtIndex0F, v5f)) should equal(1.0)
          v1Factor.get(List(vtIndex1T, v5t)) should equal(1.0)
          v1Factor.get(List(vtIndex1F, v5t)) should equal(1.0)
          v1Factor.get(List(vtIndex1T, v5f)) should equal(1.0)
          v1Factor.get(List(vtIndex1F, v5f)) should equal(1.0)
        }
    }

  "given a chain with parent without * using the old chain method" should {
      "produce a conditional selector for each parent value" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Flip(0.2)
        val v2 = Select(0.1 -> 1, 0.9 -> 2)
        val v3 = Constant(3)
        val v4 = Chain(v1, (b: Boolean) => if (b) v2; else v3)
        pr.add(v1)
        pr.add(v4)
        val c1 = cc(v1)
        val c4 = cc(v4)
        c1.generateRange()
        c4.expand()
        val c2 = cc(v2)
        val c3 = cc(v3)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v4Vals = c3.variable.range
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val tupleFactor :: selectors = c4.nonConstraintFactors
        selectors.size should equal (2)
        val (trueSelector, falseSelector) =
          if (v1t == 0) {
            assert(v1f == 1)
            (selectors(0), selectors(1))
        } else {
            assert(v1t == 1)
            assert(v1f == 0)
            (selectors(1), selectors(0))
        }
        tupleFactor.variables(0) should equal (c1.variable)
        tupleFactor.variables(1) should equal (c4.variable)
        val tupleVar = tupleFactor.variables(2)
        trueSelector.variables.size should equal (2)
        trueSelector.variables(0) should equal (tupleVar)
        trueSelector.variables(1).range should equal (c2.variable.range)
        falseSelector.variables.size should equal (2)
        falseSelector.variables(0) should equal (tupleVar)
        falseSelector.variables(1).range should equal (c3.variable.range)
        val v1TrueIndex = c1.variable.range.indexOf(Regular(true))
        val v1FalseIndex = c1.variable.range.indexOf(Regular(false))
        val v21Index = c2.variable.range.indexOf(Regular(1))
        val v22Index = c2.variable.range.indexOf(Regular(2))
        val v41Index = c4.variable.range.indexOf(Regular(1))
        val v42Index = c4.variable.range.indexOf(Regular(2))
        val v43Index = c4.variable.range.indexOf(Regular(3))
        val vtIndexT1 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(1))))
        val vtIndexF1 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(1))))
        val vtIndexT2 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(2))))
        val vtIndexF2 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(2))))
        val vtIndexT3 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(3))))
        val vtIndexF3 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(3))))
        trueSelector.get(List(vtIndexT1, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexT1, v22Index)) should equal (0.0)
        trueSelector.get(List(vtIndexT2, v21Index)) should equal (0.0)
        trueSelector.get(List(vtIndexT2, v22Index)) should equal (1.0)
        trueSelector.get(List(vtIndexT3, v21Index)) should equal (0.0)
        trueSelector.get(List(vtIndexT3, v22Index)) should equal (0.0)
        trueSelector.get(List(vtIndexF1, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF1, v22Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF2, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF2, v22Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF3, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF3, v22Index)) should equal (1.0)
        falseSelector.get(List(vtIndexT1, 0)) should equal (1.0)
        falseSelector.get(List(vtIndexT2, 0)) should equal (1.0)
        falseSelector.get(List(vtIndexT3, 0)) should equal (1.0)
        falseSelector.get(List(vtIndexF1, 0)) should equal (0.0)
        falseSelector.get(List(vtIndexF2, 0)) should equal (0.0)
        falseSelector.get(List(vtIndexF3, 0)) should equal (1.0)
      }

      "produce a conditional selector for each non-temporary parent value" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Flip(0.2)
        val v4 = Chain(v1, (b: Boolean) => if (b) Select(0.1 -> 1, 0.9 -> 2); else Constant(3))
        pr.add(v1)
        pr.add(v4)
        val c1 = cc(v1)
        val c4 = cc(v4)
        c1.generateRange()
        c4.expand()
        val v2 = cc.expansions((v4.chainFunction, true)).target
        val v3 = cc.expansions((v4.chainFunction, false)).target
        val c2 = cc(v2)
        val c3 = cc(v3)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v4Vals = c3.variable.range
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val tupleFactor :: selectors = c4.nonConstraintFactors
        selectors.size should equal (2)
        val (trueSelector, falseSelector) =
          if (v1t == 0) {
            assert(v1f == 1)
            (selectors(0), selectors(1))
        } else {
            assert(v1t == 1)
            assert(v1f == 0)
            (selectors(1), selectors(0))
        }
        tupleFactor.variables(0) should equal (c1.variable)
        tupleFactor.variables(1) should equal (c4.variable)
        val tupleVar = tupleFactor.variables(2)
        trueSelector.variables.size should equal (2)
        trueSelector.variables(0) should equal (tupleVar)
        trueSelector.variables(1).range should equal (c2.variable.range)
        falseSelector.variables.size should equal (2)
        falseSelector.variables(0) should equal (tupleVar)
        falseSelector.variables(1).range should equal (c3.variable.range)
        trueSelector.size should equal (12)
        falseSelector.size should equal (6)
        val v1TrueIndex = c1.variable.range.indexOf(Regular(true))
        val v1FalseIndex = c1.variable.range.indexOf(Regular(false))
        val v21Index = c2.variable.range.indexOf(Regular(1))
        val v22Index = c2.variable.range.indexOf(Regular(2))
        val v41Index = c4.variable.range.indexOf(Regular(1))
        val v42Index = c4.variable.range.indexOf(Regular(2))
        val v43Index = c4.variable.range.indexOf(Regular(3))
        val vtIndexT1 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(1))))
        val vtIndexT2 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(2))))
        val vtIndexT3 = tupleVar.range.indexOf(Regular(List(Regular(true), Regular(3))))
        val vtIndexF1 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(1))))
        val vtIndexF2 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(2))))
        val vtIndexF3 = tupleVar.range.indexOf(Regular(List(Regular(false), Regular(3))))
        trueSelector.get(List(vtIndexT1, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexT1, v22Index)) should equal (0.0)
        trueSelector.get(List(vtIndexT2, v21Index)) should equal (0.0)
        trueSelector.get(List(vtIndexT2, v22Index)) should equal (1.0)
        trueSelector.get(List(vtIndexT3, v21Index)) should equal (0.0)
        trueSelector.get(List(vtIndexT3, v22Index)) should equal (0.0)
        trueSelector.get(List(vtIndexF1, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF1, v22Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF2, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF2, v22Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF3, v21Index)) should equal (1.0)
        trueSelector.get(List(vtIndexF3, v22Index)) should equal (1.0)
        falseSelector.get(List(vtIndexT1, 0)) should equal (1.0)
        falseSelector.get(List(vtIndexT2, 0)) should equal (1.0)
        falseSelector.get(List(vtIndexT3, 0)) should equal (1.0)
        falseSelector.get(List(vtIndexF1, 0)) should equal (0.0)
        falseSelector.get(List(vtIndexF2, 0)) should equal (0.0)
        falseSelector.get(List(vtIndexF3, 0)) should equal (1.0)
      }
    }

    "given a chain with parent with * using the old chain method" should {
      "produce an appropriate conditional selector for the * parent value" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(true)
        val v2 = Constant(false)
        val v3 = Uniform(v1, v2)
        val v4 = Select(0.1 -> 1, 0.9 -> 2)
        val v5 = Constant(3)
        val v6 = Chain(v3, (b: Boolean) => if (b) v4; else v5)
        pr.add(v2)
        pr.add(v3)
        pr.add(v6)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c6 = cc(v6)
        c2.generateRange()
        c3.generateRange()
        c6.expand()
        val c5 = cc(v5)
        c5.generateRange()
        c6.generateRange()
        c6.makeNonConstraintFactors()

        val v3Star = c3.variable.range.indexWhere(!_.isRegular)
        val tupleFactor :: selectors = c6.nonConstraintFactors
        val tupleVar = tupleFactor.variables(2)
        val starSelector = selectors(v3Star)
        starSelector.variables.size should equal (2)
        starSelector.variables(0) should equal (tupleVar)
        starSelector.variables(1).range.size should equal (1)
        starSelector.variables(1).range(0).isRegular should equal (false)
      }
    }

  "given a chain with no globals, no *, and the same values for each parent value, using the new chain method" should {
    "produce a single factor connecting parent and child" in {
      Universe.createNew()
      val cc = new ComponentCollection
      cc.useSingleChainFactor = true
      val pr = new Problem(cc)
      val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
      val v2 = Chain(v1, (i: Int) => Flip(i / 10.0))
      pr.add(v1)
      pr.add(v2)
      val c1 = cc(v1)
      val c2 = cc(v2)
      c1.generateRange()
      c2.expand()

      val pr1 = cc.expansions(v2.chainFunction, 1)
      val subV1 = pr1.target
      val subC1 = cc(subV1)
      subC1.generateRange()
      subC1.makeNonConstraintFactors()
      pr1.solve(new ConstantStrategy(marginalVariableElimination))

      val pr2 = cc.expansions(v2.chainFunction, 2)
      val subV2 = pr2.target
      val subC2 = cc(subV2)
      subC2.generateRange()
      subC2.makeNonConstraintFactors()
      pr2.solve(new ConstantStrategy(marginalVariableElimination))
      val pr3 = cc.expansions(v2.chainFunction, 3)
      val subV3 = pr3.target
      val subC3 = cc(subV3)
      subC3.generateRange()
      subC3.makeNonConstraintFactors()
      pr3.solve(new ConstantStrategy(marginalVariableElimination))

      c2.generateRange()
      c2.makeNonConstraintFactors()

      val List(factor) = c2.nonConstraintFactors
      factor.variables should equal (List(c1.variable, c2.variable))
      val v1Vals = c1.variable.range
      val v2Vals = c2.variable.range
      val v11 = v1Vals indexOf Regular(1)
      val v12 = v1Vals indexOf Regular(2)
      val v13 = v1Vals indexOf Regular(3)
      val v2f = v2Vals indexOf Regular(false)
      val v2t = v2Vals indexOf Regular(true)
      factor.size should equal (6)
      factor.get(List(v11,v2f)) should be (0.9 +- 0.0001)
      factor.get(List(v11,v2t)) should be (0.1 +- 0.0001)
      factor.get(List(v12,v2f)) should be (0.8 +- 0.0001)
      factor.get(List(v12,v2t)) should be (0.2 +- 0.0001)
      factor.get(List(v13,v2f)) should be (0.7 +- 0.0001)
      factor.get(List(v13,v2t)) should be (0.3 +- 0.0001)
    }
  }

  "given a chain with no globals, no *, different values for each parent value, using new chain method" should {
    "produce a factor mapping the parent to the child with the union of the values" in {
      Universe.createNew()
      val cc = new ComponentCollection
      cc.useSingleChainFactor = true
      val pr = new Problem(cc)
      val v1 = Flip(0.5)
      val v2 = Chain(v1, (b: Boolean) => if (b) Select(0.1 -> 1, 0.9 -> 2) else Select(0.2 -> 2, 0.8 -> 3))
      pr.add(v1)
      pr.add(v2)
      val c1 = cc(v1)
      val c2 = cc(v2)
      c1.generateRange()
      c2.expand()

      val prf = cc.expansions(v2.chainFunction, false)
      val subVf = prf.target
      val subCf = cc(subVf)
      subCf.generateRange()
      subCf.makeNonConstraintFactors()
      prf.solve(new ConstantStrategy(marginalVariableElimination))
      val prt = cc.expansions(v2.chainFunction, true)
      val subVt = prt.target
      val subCt = cc(subVt)
      subCt.generateRange()
      subCt.makeNonConstraintFactors()
      prt.solve(new ConstantStrategy(marginalVariableElimination))

      c2.generateRange()
      c2.makeNonConstraintFactors()

      val List(factor) = c2.nonConstraintFactors
      factor.variables should equal (List(c1.variable, c2.variable))
      val v1Vals = c1.variable.range
      val v2Vals = c2.variable.range
      val v1f = v1Vals indexOf Regular(false)
      val v1t = v1Vals indexOf Regular(true)
      val v21 = v2Vals indexOf Regular(1)
      val v22 = v2Vals indexOf Regular(2)
      val v23 = v2Vals indexOf Regular(3)
      factor.size should equal (6)
      factor.get(List(v1f,v21)) should be (0.0 +- 0.0001)
      factor.get(List(v1f,v22)) should be (0.2 +- 0.0001)
      factor.get(List(v1f,v23)) should be (0.8 +- 0.0001)
      factor.get(List(v1t,v21)) should be (0.1 +- 0.0001)
      factor.get(List(v1t,v22)) should be (0.9 +- 0.0001)
      factor.get(List(v1t,v23)) should be (0.0 +- 0.0001)

    }
  }

  "given a chain with no globals, * in subproblem, using new chain method" should {
    "produce a factor mapping parent to child including *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      cc.useSingleChainFactor = true
      val pr = new Problem(cc)
      val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
      val v2 = Chain(v1, (i: Int) => Flip(i / 10.0))
      pr.add(v1)
      pr.add(v2)
      val c1 = cc(v1)
      val c2 = cc(v2)
      c1.generateRange()
      c2.expand()

      val pr1 = cc.expansions(v2.chainFunction, 1)
      val subV1 = pr1.target
      val subC1 = cc(subV1)
      subC1.generateRange()
      subC1.makeNonConstraintFactors()
      pr1.solve(new ConstantStrategy(marginalVariableElimination))
      val pr2 = cc.expansions(v2.chainFunction, 2)
      val subV2 = pr2.target
      val subC2 = cc(subV2)
      subC2.generateRange()
      subC2.makeNonConstraintFactors()
      pr2.solve(new ConstantStrategy(marginalVariableElimination))
      val pr3 = cc.expansions(v2.chainFunction, 3)
      // no range generation or factor creation
      pr3.solve(new ConstantStrategy(marginalVariableElimination))

      c2.generateRange()
      c2.makeNonConstraintFactors()

      val List(factor) = c2.nonConstraintFactors
      factor.variables should equal (List(c1.variable, c2.variable))
      val v1Vals = c1.variable.range
      val v2Vals = c2.variable.range
      val v11 = v1Vals indexOf Regular(1)
      val v12 = v1Vals indexOf Regular(2)
      val v13 = v1Vals indexOf Regular(3)
      val v2f = v2Vals indexOf Regular(false)
      val v2t = v2Vals indexOf Regular(true)
      val v2Star = v2Vals indexWhere (!_.isRegular)
      factor.size should equal (9)
      factor.get(List(v11,v2f)) should be (0.9 +- 0.0001)
      factor.get(List(v11,v2t)) should be (0.1 +- 0.0001)
      factor.get(List(v11,v2Star)) should be (0.0 +- 0.0001)
      factor.get(List(v12,v2f)) should be (0.8 +- 0.0001)
      factor.get(List(v12,v2t)) should be (0.2 +- 0.0001)
      factor.get(List(v12,v2Star)) should be (0.0 +- 0.0001)
      factor.get(List(v13,v2f)) should be (0.0 +- 0.0001)
      factor.get(List(v13,v2t)) should be (0.0 +- 0.0001)
      factor.get(List(v13,v2Star)) should be (1.0 +- 0.0001)

    }
  }

  "given a chain with no globals, * in parent values, using new chain method" should {
    "produce a factor mapping parent to child including *" in {
      Universe.createNew()
      val cc = new ComponentCollection
      cc.useSingleChainFactor = true
      val pr = new Problem(cc)
      val vt = Constant(true)
      val vf = Constant(false)
      val v1 = Uniform(vt, vf)
      val v2 = Chain(v1, (b: Boolean) => if (b) Select(0.1 -> 1, 0.9 -> 2) else Select(0.2 -> 1, 0.8 -> 2))
      pr.add(vt)
      pr.add(vf)
      pr.add(v1)
      pr.add(v2)
      val ct = cc(vt)
      val cf = cc(vf)
      val c1 = cc(v1)
      val c2 = cc(v2)
      ct.generateRange()
      ct.makeNonConstraintFactors()
      // do not generate range for cf
      c1.generateRange() // will include true and *
      c1.makeNonConstraintFactors()
      c2.expand()

      val prt = cc.expansions(v2.chainFunction, true)
      val subVt = prt.target
      val subCt = cc(subVt)
      subCt.generateRange()
      subCt.makeNonConstraintFactors()
      prt.solve(new ConstantStrategy(marginalVariableElimination))

      c2.generateRange()
      c2.makeNonConstraintFactors()

      val List(factor) = c2.nonConstraintFactors
      factor.variables should equal (List(c1.variable, c2.variable))
      val v1Vals = c1.variable.range
      val v2Vals = c2.variable.range
      val v1Star = v1Vals.indexWhere(!_.isRegular)
      val v1t = v1Vals indexOf Regular(true)
      val v21 = v2Vals indexOf Regular(1)
      val v22 = v2Vals indexOf Regular(2)
      val v2Star = v2Vals.indexWhere(!_.isRegular)
      factor.size should equal (6)
      factor.get(List(v1Star,v21)) should be (0.0 +- 0.0001)
      factor.get(List(v1Star,v22)) should be (0.0 +- 0.0001)
      factor.get(List(v1Star,v2Star)) should be (1.0 +- 0.0001)
      factor.get(List(v1t,v21)) should be (0.1 +- 0.0001)
      factor.get(List(v1t,v22)) should be (0.9 +- 0.0001)
      factor.get(List(v1t,v2Star)) should be (0.0 +- 0.0001)
    }
  }

  "given a chain with globals" should {
    "not use the new method even when the new method is being used" in {
      Universe.createNew()
      val cc = new ComponentCollection
      cc.useSingleChainFactor = true
      val pr = new Problem(cc)
      val v1 = Constant(1)
      val v2 = Flip(0.5)
      val v3 = Chain(v2, (b: Boolean) => if (b) Apply(v1, (i: Int) => i) else Constant(2))
      pr.add(v1)
      pr.add(v2)
      pr.add(v3)
      val c1 = cc(v1)
      val c2 = cc(v2)
      val c3 = cc(v3)
      c1.generateRange()
      c2.generateRange()
      c3.expand()
      val subPf = cc.expansions(v3.chainFunction, false)
      val vPf = subPf.target
      val cPf = cc(vPf)
      cPf.generateRange()
      cPf.makeNonConstraintFactors()
      subPf.solve(new ConstantStrategy(marginalVariableElimination))
      val subPt = cc.expansions(v3.chainFunction, true)
      val vPt = subPt.target
      val cPt = cc(vPt)
      cPt.generateRange()
      cPt.makeNonConstraintFactors()
      subPt.solve(new ConstantStrategy(marginalVariableElimination))
      c3.generateRange()
      c3.makeNonConstraintFactors()

      c3.nonConstraintFactors.length should be > (1)
    }
  }

  "given a chain in which the outcome is a global, using the new method" should {
    "produce the right result" in {
      Universe.createNew()
      val cc = new ComponentCollection
      cc.useSingleChainFactor = true
      val pr = new Problem(cc)
      val v1 = Constant(1)
      val v2 = Flip(0.5)
      val v3 = Chain(v2, (b: Boolean) => if (b) v1 else Constant(2))
      StructuredVE.probability(v3, 1) should equal (0.5)
    }
  }

    "given an apply of one argument without *" should {
      "produce a sparse factor that matches the argument to the result via the function" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Apply(v1, (i: Int) => i % 2)
        pr.add(v1)
        pr.add(v2)
        val c1 = cc(v1)
        val c2 = cc(v2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors()

        val List(factor) = c2.nonConstraintFactors
        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v20 = v2Vals indexOf Regular(0)
        val v21 = v2Vals indexOf Regular(1)
        factor.contains(List(v11, v20)) should equal(false)
        factor.get(List(v11, v21)) should equal(1.0)
        factor.get(List(v12, v20)) should equal(1.0)
        factor.contains(List(v12, v21)) should equal(false)
        factor.contains(List(v13, v20)) should equal(false)
        factor.get(List(v13, v21)) should equal(1.0)
        factor.get(List(v12, v20)) should equal (1.0)
        factor.contents.size should equal(3)
      }
    }

    "given an apply of one argument with *" should {
      "produce a sparse factor that matches regular values to the function result and matches * to *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(1)
        val v2 = Constant(2)
        val v3 = Constant(3)
        val v4 = Dist(0.3 -> v1, 0.2 -> v2, 0.5 -> v3)
        val v5 = Apply(v4, (i: Int) => i % 2)
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c5.makeNonConstraintFactors()

        val List(factor) = c5.nonConstraintFactors
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v4Star = v4Vals indexWhere(!_.isRegular)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val v50 = v5Vals indexOf Regular(0)
        val v51 = v5Vals indexOf Regular(1)
        val v5Star = v5Vals indexWhere(!_.isRegular)
        factor.contains(List(v4Star, v50)) should equal(false)
        factor.contains(List(v4Star, v51)) should equal(false)
        factor.get(List(v4Star, v5Star)) should equal(1.0)
        factor.contains(List(v42, v51)) should equal(false)
        factor.contains(List(v43, v50)) should equal(false)
        factor.contains(List(v42, v5Star)) should equal(false)
        factor.contains(List(v43, v5Star)) should equal(false)
        factor.get(List(v43, v51)) should equal(1.0)
        factor.get(List(v42, v50)) should equal (1.0)
        factor.contents.size should equal(3)
      }
    }

    "given an apply of two arguments without *" should {
      "produce a sparse factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 2, 0.5 -> 3)
        val v3 = Apply(v1, v2, (i: Int, j: Int) => i % j)
        pr.add(v1)
        pr.add(v2)
        pr.add(v3)
        val c1 = cc(v1)
        val c2 = cc(v2)
        val c3 = cc(v3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c3.makeNonConstraintFactors()

        val List(factor) = c3.nonConstraintFactors
        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v3Vals = c3.variable.range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v22 = v2Vals indexOf Regular(2)
        val v23 = v2Vals indexOf Regular(3)
        val v30 = v3Vals indexOf Regular(0)
        val v31 = v3Vals indexOf Regular(1)
        val v32 = v3Vals indexOf Regular(2)
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

    "given an apply of the same two arguments" should {
      "produce a deduplicate factord with no repeated variables" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Flip(0.5)
        val e2 = Apply(e1, e1, (b1: Boolean, b2: Boolean) => if (b1 == b2) 1 else 2)
        pr.add(e1)
        pr.add(e2)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors()

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c1.variable, c2.variable))
        val c1IndexT = c1.variable.range.indexOf(Regular(true))
        val c1IndexF = c1.variable.range.indexOf(Regular(false))
        val c2Index1 = c2.variable.range.indexOf(Regular(1))
        val c2Index2 = c2.variable.range.indexOf(Regular(2))
        factor.get(List(c1IndexT, c2Index1)) should equal (1.0)
        factor.get(List(c1IndexT, c2Index2)) should equal (0.0)
        factor.get(List(c1IndexF, c2Index1)) should equal (1.0)
        factor.get(List(c1IndexF, c2Index2)) should equal (0.0)
      }
    }

    "given an apply of two arguments with *" should {
      "produce a sparse factor that matches regular arguments to the function result and matches * to *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(1)
        val v2 = Constant(2)
        val v3 = Constant(3)
        val v4 = Dist(0.3 -> v1, 0.2 -> v2, 0.5 -> v3)
        val v5 = Select(0.5 -> 2, 0.5 -> 3)
        val v6 = Apply(v4, v5, (i: Int, j: Int) => i % j)
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        pr.add(v6)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        val c6 = cc(v6)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c6.makeNonConstraintFactors()

        val List(factor) = c6.nonConstraintFactors
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v6Vals = c6.variable.range
        val v4Star = v4Vals indexWhere(!_.isRegular)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val v52 = v5Vals indexOf Regular(2)
        val v53 = v5Vals indexOf Regular(3)
        val v6Star = v6Vals indexWhere(!_.isRegular)
        val v60 = v6Vals indexOf Regular(0)
        val v61 = v6Vals indexOf Regular(1)
        val v62 = v6Vals indexOf Regular(2)
        factor.contains(List(v4Star, v52, v60)) should equal(false)
        factor.contains(List(v4Star, v52, v61)) should equal(false)
        factor.contains(List(v4Star, v52, v62)) should equal(false)
        factor.get(List(v4Star, v52, v6Star)) should equal(1.0)
        factor.contains(List(v4Star, v53, v60)) should equal(false)
        factor.contains(List(v4Star, v53, v61)) should equal(false)
        factor.contains(List(v4Star, v53, v62)) should equal(false)
        factor.get(List(v4Star, v53, v6Star)) should equal(1.0)
        factor.get(List(v42, v52, v60)) should equal(1.0)
        factor.contains(List(v42, v52, v61)) should equal(false)
        factor.contains(List(v42, v52, v62)) should equal(false)
        factor.contains(List(v42, v52, v6Star)) should equal(false)
        factor.contains(List(v42, v53, v60)) should equal(false)
        factor.contains(List(v42, v53, v61)) should equal(false)
        factor.get(List(v42, v53, v62)) should equal(1.0)
        factor.contains(List(v42, v53, v6Star)) should equal(false)
        factor.contains(List(v43, v52, v60)) should equal(false)
        factor.get(List(v43, v52, v61)) should equal(1.0)
        factor.contains(List(v43, v52, v62)) should equal(false)
        factor.contains(List(v43, v52, v6Star)) should equal(false)
        factor.get(List(v43, v53, v60)) should equal(1.0)
        factor.contains(List(v43, v53, v61)) should equal(false)
        factor.contains(List(v43, v53, v62)) should equal(false)
        factor.contains(List(v43, v53, v6Star)) should equal(false)
        factor.contents.size should equal(6)
      }
    }

    "given an apply of three arguments without *" should {
      "produce a sparse factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4: Apply3[Int, Int, Int, Int] = Apply(v1, v2, v3, (i: Int, j: Int, k: Int) => i % (j + k))
        pr.add(v1)
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        val c1 = cc(v1)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val List(factor) = c4.nonConstraintFactors
        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v3Vals = c3.variable.range
        val v4Vals = c4.variable.range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v31 = v3Vals indexOf Regular(1)
        val v40 = v4Vals indexOf Regular(0)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
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

    "given an apply of three arguments with *" should {
      "produce a sparse factor that matches regular arguments to function results and matches * to *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(1)
        val v2 = Constant(2)
        val v3 = Constant(3)
        val v4 = Dist(0.3 -> v1, 0.2 -> v2, 0.5 -> v3)
        val v5 = Select(0.5 -> 1, 0.5 -> 2)
        val v6 = Constant(1)
        val v7: Apply3[Int, Int, Int, Int] = Apply(v4, v5, v6, (i: Int, j: Int, k: Int) => i % (j + k))
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        pr.add(v6)
        pr.add(v7)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        val c6 = cc(v6)
        val c7 = cc(v7)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c7.generateRange()
        c7.makeNonConstraintFactors()

        val List(factor) = c7.nonConstraintFactors
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v6Vals = c6.variable.range
        val v7Vals = c7.variable.range
        val v4Star = v4Vals indexWhere (!_.isRegular)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val v51 = v5Vals indexOf Regular(1)
        val v52 = v5Vals indexOf Regular(2)
        val v61 = v6Vals indexOf Regular(1)
        val v70 = v7Vals indexOf Regular(0)
        val v71 = v7Vals indexOf Regular(1)
        val v72 = v7Vals indexOf Regular(2)
        val v7Star = v7Vals indexWhere (!_.isRegular)
        factor.contains(List(v4Star, v51, v61, v70)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v71)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v72)) should equal(false)
        factor.get(List(v4Star, v51, v61, v7Star)) should equal(1.0)
        factor.contains(List(v4Star, v52, v61, v70)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v71)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v72)) should equal(false)
        factor.get(List(v4Star, v52, v61, v7Star)) should equal(1.0)
        factor.get(List(v42, v51, v61, v70)) should equal(1.0)
        factor.contains(List(v42, v51, v61, v71)) should equal(false)
        factor.contains(List(v42, v51, v61, v72)) should equal(false)
        factor.contains(List(v42, v51, v61, v7Star)) should equal(false)
        factor.contains(List(v42, v52, v61, v70)) should equal(false)
        factor.contains(List(v42, v52, v61, v71)) should equal(false)
        factor.get(List(v42, v52, v61, v72)) should equal(1.0)
        factor.contains(List(v42, v52, v61, v7Star)) should equal(false)
        factor.contains(List(v43, v51, v61, v70)) should equal(false)
        factor.get(List(v43, v51, v61, v71)) should equal(1.0)
        factor.contains(List(v43, v51, v61, v72)) should equal(false)
        factor.contains(List(v43, v51, v61, v7Star)) should equal(false)
        factor.get(List(v43, v52, v61, v70)) should equal(1.0)
        factor.contains(List(v43, v52, v61, v71)) should equal(false)
        factor.contains(List(v43, v52, v61, v72)) should equal(false)
        factor.contains(List(v43, v52, v61, v7Star)) should equal(false)
        factor.contents.size should equal(6)
      }
    }

    "given an apply of four arguments without *" should {
      "produce a sparse factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4 = Flip(0.7)
        val v5: Apply4[Int, Int, Int, Boolean, Int] =
          Apply(v1, v2, v3, v4, (i: Int, j: Int, k: Int, b: Boolean) => if (b) 0; else i % (j + k))
        pr.add(v1)
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        val c1 = cc(v1)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c5.makeNonConstraintFactors()

        val List(factor) = c5.nonConstraintFactors
        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v3Vals = c3.variable.range
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
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

    "given an apply of four arguments with *" should {
      "produce a sparse factor that matches regular arguments to function results and matches * to *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(1)
        val v2 = Constant(2)
        val v3 = Constant(3)
        val v4 = Dist(0.3 -> v1, 0.2 -> v2, 0.5 -> v3)
        val v5 = Select(0.5 -> 1, 0.5 -> 2)
        val v6 = Constant(1)
        val v7 = Flip(0.7)
        val v8: Apply4[Int, Int, Int, Boolean, Int] =
          Apply(v4, v5, v6, v7, (i: Int, j: Int, k: Int, b: Boolean) => if (b) 0; else i % (j + k))
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        pr.add(v6)
        pr.add(v7)
        pr.add(v8)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        val c6 = cc(v6)
        val c7 = cc(v7)
        val c8 = cc(v8)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c7.generateRange()
        c8.generateRange()
        c8.makeNonConstraintFactors()

        val List(factor) = c8.nonConstraintFactors
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v6Vals = c6.variable.range
        val v7Vals = c7.variable.range
        val v8Vals = c8.variable.range
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val v4Star = v4Vals indexWhere (!_.isRegular)
        val v51 = v5Vals indexOf Regular(1)
        val v52 = v5Vals indexOf Regular(2)
        val v61 = v6Vals indexOf Regular(1)
        val v7true = v7Vals indexOf Regular(true)
        val v7false = v7Vals indexOf Regular(false)
        val v80 = v8Vals indexOf Regular(0)
        val v81 = v8Vals indexOf Regular(1)
        val v82 = v8Vals indexOf Regular(2)
        val v8Star = v8Vals indexWhere (!_.isRegular)
        factor.contains(List(v4Star, v51, v61, v7false, v80)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7false, v81)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7false, v82)) should equal(false)
        factor.get(List(v4Star, v51, v61, v7false, v8Star)) should equal(1.0)
        factor.contains(List(v4Star, v52, v61, v7false, v80)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7false, v81)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7false, v82)) should equal(false)
        factor.get(List(v4Star, v52, v61, v7false, v8Star)) should equal(1.0)
        factor.get(List(v42, v51, v61, v7false, v80)) should equal(1.0)
        factor.contains(List(v42, v51, v61, v7false, v81)) should equal(false)
        factor.contains(List(v42, v51, v61, v7false, v82)) should equal(false)
        factor.contains(List(v42, v51, v61, v7false, v8Star)) should equal(false)
        factor.contains(List(v42, v52, v61, v7false, v80)) should equal(false)
        factor.contains(List(v42, v52, v61, v7false, v81)) should equal(false)
        factor.get(List(v42, v52, v61, v7false, v82)) should equal(1.0)
        factor.contains(List(v42, v52, v61, v7false, v8Star)) should equal(false)
        factor.contains(List(v43, v51, v61, v7false, v80)) should equal(false)
        factor.get(List(v43, v51, v61, v7false, v81)) should equal(1.0)
        factor.contains(List(v43, v51, v61, v7false, v82)) should equal(false)
        factor.contains(List(v43, v51, v61, v7false, v8Star)) should equal(false)
        factor.get(List(v43, v52, v61, v7false, v80)) should equal(1.0)
        factor.contains(List(v43, v52, v61, v7false, v81)) should equal(false)
        factor.contains(List(v43, v52, v61, v7false, v82)) should equal(false)
        factor.contains(List(v43, v52, v61, v7false, v8Star)) should equal(false)

        factor.contains(List(v4Star, v51, v61, v7true, v80)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7true, v81)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7true, v82)) should equal(false)
        factor.get(List(v4Star, v51, v61, v7true, v8Star)) should equal(1.0)
        factor.contains(List(v4Star, v52, v61, v7true, v80)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7true, v81)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7true, v82)) should equal(false)
        factor.get(List(v4Star, v52, v61, v7true, v8Star)) should equal(1.0)
        factor.get(List(v42, v51, v61, v7true, v80)) should equal(1.0)
        factor.contains(List(v42, v51, v61, v7true, v81)) should equal(false)
        factor.contains(List(v42, v51, v61, v7true, v82)) should equal(false)
        factor.contains(List(v42, v51, v61, v7true, v8Star)) should equal(false)
        factor.get(List(v42, v52, v61, v7true, v80)) should equal(1.0)
        factor.contains(List(v42, v52, v61, v7true, v81)) should equal(false)
        factor.contains(List(v42, v52, v61, v7true, v82)) should equal(false)
        factor.contains(List(v42, v52, v61, v7true, v8Star)) should equal(false)
        factor.get(List(v43, v51, v61, v7true, v80)) should equal(1.0)
        factor.contains(List(v43, v51, v61, v7true, v81)) should equal(false)
        factor.contains(List(v43, v51, v61, v7true, v82)) should equal(false)
        factor.contains(List(v43, v51, v61, v7true, v8Star)) should equal(false)
        factor.get(List(v43, v52, v61, v7true, v80)) should equal(1.0)
        factor.contains(List(v43, v52, v61, v7true, v81)) should equal(false)
        factor.contains(List(v43, v52, v61, v7true, v82)) should equal(false)
        factor.contains(List(v43, v52, v61, v7true, v8Star)) should equal(false)
        factor.contents.size should equal(12)
      }
    }

    "given an apply of five arguments without *" should {
      "produce a sparse factor that matches the arguments to the result via the function" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 1, 0.5 -> 2)
        val v3 = Constant(1)
        val v4 = Flip(0.7)
        val v5 = Constant(false)
        val v6: Apply5[Int, Int, Int, Boolean, Boolean, Int] =
          Apply(v1, v2, v3, v4, v5,
            (i: Int, j: Int, k: Int, b: Boolean, c: Boolean) => if (b || c) 0; else i % (j + k))
        pr.add(v1)
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        pr.add(v6)
        val c1 = cc(v1)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        val c6 = cc(v6)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c6.makeNonConstraintFactors()

        val List(factor) = c6.nonConstraintFactors
        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v3Vals = c3.variable.range
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v6Vals = c6.variable.range
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

    "given an apply of five arguments with *" should {
      "produce a sparse factor that matches regular arguments to function results and matches * to *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(1)
        val v2 = Constant(2)
        val v3 = Constant(3)
        val v4 = Dist(0.3 -> v1, 0.2 -> v2, 0.5 -> v3)
        val v5 = Select(0.5 -> 1, 0.5 -> 2)
        val v6 = Constant(1)
        val v7 = Flip(0.7)
        val v8 = Constant(false)
        val v9: Apply5[Int, Int, Int, Boolean, Boolean, Int] =
          Apply(v4, v5, v6, v7, v8,
            (i: Int, j: Int, k: Int, b: Boolean, c: Boolean) => if (b || c) 0; else i % (j + k))
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        pr.add(v6)
        pr.add(v7)
        pr.add(v8)
        pr.add(v9)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        val c6 = cc(v6)
        val c7 = cc(v7)
        val c8 = cc(v8)
        val c9 = cc(v9)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c7.generateRange()
        c8.generateRange()
        c9.generateRange()
        c9.makeNonConstraintFactors()

        val List(factor) = c9.nonConstraintFactors
        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v6Vals = c6.variable.range
        val v7Vals = c7.variable.range
        val v8Vals = c8.variable.range
        val v9Vals = c9.variable.range
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val v4Star = v4Vals.indexWhere(!_.isRegular)
        val v51 = v5Vals indexOf Regular(1)
        val v52 = v5Vals indexOf Regular(2)
        val v61 = v6Vals indexOf Regular(1)
        val v7true = v7Vals indexOf Regular(true)
        val v7false = v7Vals indexOf Regular(false)
        val v8false = v8Vals indexOf Regular(false)
        val v90 = v9Vals indexOf Regular(0)
        val v91 = v9Vals indexOf Regular(1)
        val v9Star = v9Vals.indexWhere(!_.isRegular)
        val v92 = v9Vals indexOf Regular(2)

        factor.contains(List(v4Star, v51, v61, v7false, v8false, v90)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7false, v8false, v91)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7false, v8false, v92)) should equal(false)
        factor.get(List(v4Star, v51, v61, v7false, v8false, v9Star)) should equal(1.0)
        factor.contains(List(v4Star, v52, v61, v7false, v8false, v90)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7false, v8false, v91)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7false, v8false, v92)) should equal(false)
        factor.get(List(v4Star, v52, v61, v7false, v8false, v9Star)) should equal(1.0)
        factor.get(List(v42, v51, v61, v7false, v8false, v90)) should equal(1.0)
        factor.contains(List(v42, v51, v61, v7false, v8false, v91)) should equal(false)
        factor.contains(List(v42, v51, v61, v7false, v8false, v92)) should equal(false)
        factor.contains(List(v42, v51, v61, v7false, v8false, v9Star)) should equal(false)
        factor.contains(List(v42, v52, v61, v7false, v8false, v90)) should equal(false)
        factor.contains(List(v42, v52, v61, v7false, v8false, v91)) should equal(false)
        factor.get(List(v42, v52, v61, v7false, v8false, v92)) should equal(1.0)
        factor.contains(List(v42, v52, v61, v7false, v8false, v9Star)) should equal(false)
        factor.contains(List(v43, v51, v61, v7false, v8false, v90)) should equal(false)
        factor.get(List(v43, v51, v61, v7false, v8false, v91)) should equal(1.0)
        factor.contains(List(v43, v51, v61, v7false, v8false, v92)) should equal(false)
        factor.contains(List(v43, v51, v61, v7false, v8false, v9Star)) should equal(false)
        factor.get(List(v43, v52, v61, v7false, v8false, v90)) should equal(1.0)
        factor.contains(List(v43, v52, v61, v7false, v8false, v91)) should equal(false)
        factor.contains(List(v43, v52, v61, v7false, v8false, v92)) should equal(false)
        factor.contains(List(v43, v52, v61, v7false, v8false, v9Star)) should equal(false)

        factor.contains(List(v4Star, v51, v61, v7true, v8false, v90)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7true, v8false, v91)) should equal(false)
        factor.contains(List(v4Star, v51, v61, v7true, v8false, v92)) should equal(false)
        factor.get(List(v4Star, v51, v61, v7true, v8false, v9Star)) should equal(1.0)
        factor.contains(List(v4Star, v52, v61, v7true, v8false, v90)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7true, v8false, v91)) should equal(false)
        factor.contains(List(v4Star, v52, v61, v7true, v8false, v92)) should equal(false)
        factor.get(List(v4Star, v52, v61, v7true, v8false, v9Star)) should equal(1.0)
        factor.get(List(v42, v51, v61, v7true, v8false, v90)) should equal(1.0)
        factor.contains(List(v42, v51, v61, v7true, v8false, v91)) should equal(false)
        factor.contains(List(v42, v51, v61, v7true, v8false, v92)) should equal(false)
        factor.contains(List(v42, v51, v61, v7true, v8false, v9Star)) should equal(false)
        factor.get(List(v42, v52, v61, v7true, v8false, v90)) should equal(1.0)
        factor.contains(List(v42, v52, v61, v7true, v8false, v91)) should equal(false)
        factor.contains(List(v42, v52, v61, v7true, v8false, v92)) should equal(false)
        factor.contains(List(v42, v52, v61, v7true, v8false, v9Star)) should equal(false)
        factor.get(List(v43, v51, v61, v7true, v8false, v90)) should equal(1.0)
        factor.contains(List(v43, v51, v61, v7true, v8false, v91)) should equal(false)
        factor.contains(List(v43, v51, v61, v7true, v8false, v92)) should equal(false)
        factor.contains(List(v43, v51, v61, v7true, v8false, v9Star)) should equal(false)
        factor.get(List(v43, v52, v61, v7true, v8false, v90)) should equal(1.0)
        factor.contains(List(v43, v52, v61, v7true, v8false, v91)) should equal(false)
        factor.contains(List(v43, v52, v61, v7true, v8false, v92)) should equal(false)
        factor.contains(List(v43, v52, v61, v7true, v8false, v9Star)) should equal(false)
        factor.contents.size should equal(12)
      }
    }

    "given an Inject without *" should {
      "produce a factor that matches its inputs to the correct sequence" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.3 -> 1, 0.2 -> 2, 0.5 -> 3)
        val v2 = Select(0.5 -> 4, 0.5 -> 5)
        val v3 = Inject(v1, v2)
        pr.add(v1)
        pr.add(v2)
        pr.add(v3)
        val c1 = cc(v1)
        val c2 = cc(v2)
        val c3 = cc(v3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c3.makeNonConstraintFactors()
        val List(factor) = c3.nonConstraintFactors

        val v1Vals = c1.variable.range
        val v2Vals = c2.variable.range
        val v3Vals = c3.variable.range
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

    "given an Inject with *" should {
      "produce a factor that matches regular inputs to the correct sequence and matches * to *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Constant(1)
        val v2 = Constant(2)
        val v3 = Constant(3)
        val v4 = Dist(0.3 -> v1, 0.2 -> v2, 0.5 -> v3)
        val v5 = Select(0.5 -> 4, 0.5 -> 5)
        val v6 = Inject(v4, v5)
        pr.add(v2)
        pr.add(v3)
        pr.add(v4)
        pr.add(v5)
        pr.add(v6)
        val c2 = cc(v2)
        val c3 = cc(v3)
        val c4 = cc(v4)
        val c5 = cc(v5)
        val c6 = cc(v6)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c5.generateRange()
        c6.generateRange()
        c6.makeNonConstraintFactors()
        val List(factor) = c6.nonConstraintFactors

        val v4Vals = c4.variable.range
        val v5Vals = c5.variable.range
        val v6Vals = c6.variable.range
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)
        val v4Star = v4Vals.indexWhere(!_.isRegular)
        val v54 = v5Vals indexOf Regular(4)
        val v55 = v5Vals indexOf Regular(5)
        val v624 = v6Vals indexOf Regular(List(2, 4))
        val v625 = v6Vals indexOf Regular(List(2, 5))
        val v634 = v6Vals indexOf Regular(List(3, 4))
        val v635 = v6Vals indexOf Regular(List(3, 5))
        val v6Star = v6Vals.indexWhere(!_.isRegular)

        factor.get(List(v4Star, v54, v6Star)) should equal(1.0)
        factor.get(List(v4Star, v55, v6Star)) should equal(1.0)
        factor.get(List(v42, v54, v624)) should equal(1.0)
        factor.get(List(v42, v55, v625)) should equal(1.0)
        factor.get(List(v43, v54, v634)) should equal(1.0)
        factor.get(List(v43, v55, v635)) should equal(1.0)
        factor.get(List(v4Star, v55, v624)) should equal(0.0)
        factor.get(List(v4Star, v54, v625)) should equal(0.0)
        factor.get(List(v4Star, v55, v634)) should equal(0.0)
        factor.get(List(v4Star, v54, v635)) should equal(0.0)
        factor.get(List(v42, v54, v625)) should equal(0.0)
        factor.get(List(v42, v54, v634)) should equal(0.0)
        factor.get(List(v42, v54, v635)) should equal(0.0)
        factor.get(List(v42, v55, v624)) should equal(0.0)
        factor.get(List(v42, v55, v634)) should equal(0.0)
        factor.get(List(v42, v55, v635)) should equal(0.0)
        factor.get(List(v43, v54, v625)) should equal(0.0)
        factor.get(List(v43, v54, v624)) should equal(0.0)
        factor.get(List(v43, v54, v635)) should equal(0.0)
        factor.get(List(v43, v55, v624)) should equal(0.0)
        factor.get(List(v43, v55, v625)) should equal(0.0)
        factor.get(List(v43, v55, v634)) should equal(0.0)
      }
    }

    "given a simple single-valued reference with an added target without *" should {
      "produce the factor that matches the value of the target to the value of the reference" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Flip(0.5)("e1", universe)
        val e2 = universe.get[Boolean]("e1")
        pr.add(e1)
        pr.add(e2)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.generateRange()
        c2.makeNonConstraintFactors()

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c1.variable, c2.variable))
        factor.contents.size should equal (2)
        val v1True = c1.variable.range.indexOf(Regular(true))
        val v1False = c1.variable.range.indexOf(Regular(false))
        val v2True = c2.variable.range.indexOf(Regular(true))
        val v2False = c2.variable.range.indexOf(Regular(false))
        factor.get(List(v1True, v2True)) should equal (1.0)
        factor.get(List(v1False, v2False)) should equal (1.0)
      }
    }

    "given a simple single-valued reference with an added target with *" should {
      "produce the factor that matches the value of the target to the value of the reference and * to *" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Constant(true)
        val e2 = Constant(false)
        val e3 = Uniform(e1, e2)("e3", universe)
        val e4 = universe.get[Boolean]("e3")
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val List(factor) = c4.nonConstraintFactors
        factor.variables should equal (List(c3.variable, c4.variable))
        factor.contents.size should equal (2)
        val v3False = c3.variable.range.indexOf(Regular(false))
        val v3Star = c3.variable.range.indexWhere(!_.isRegular)
        val v4False = c4.variable.range.indexOf(Regular(false))
        val v4Star = c4.variable.range.indexWhere(!_.isRegular)
        factor.get(List(v3False, v4False)) should equal (1.0)
        factor.get(List(v3Star, v4Star)) should equal (1.0)
      }
    }

    "given an indirect single-valued reference with all required elements added and reference uncertainty without *" should {
      "produce the required conditional selector and result factors" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e11 = Constant(true)("e1", ec1)
        val e12 = Constant(false)("e1", ec2)
        val e2 = Uniform(ec1, ec2)("e2", universe)
        val e3 = universe.get[Boolean]("e2.e1")
        pr.add(e11)
        pr.add(e12)
        pr.add(e2)
        pr.add(e3)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c3.generateRange()
        c3.makeNonConstraintFactors()

        // Four factors should be produced: two conditional selectors, and one for each of the possibilities
        // The conditional selector and result factors for each parent value are produced in turn
        val tupleFactor :: factors = c3.nonConstraintFactors
        val tupleVar = tupleFactor.variables(2)
        tupleFactor.variables(1) should equal (c3.variable)
        val startVar = tupleFactor.variables(0)
        startVar.valueSet.hasStar should equal (false)
        startVar.valueSet.regularValues should equal (Set(ec1, ec2))

        factors.size should equal (4)
        val ec1Index = c2.variable.range.indexOf(Regular(ec1))
        val ec2Index = c2.variable.range.indexOf(Regular(ec2))
        val e3TrueIndex = c3.variable.range.indexOf(Regular(true))
        val e3FalseIndex = c3.variable.range.indexOf(Regular(false))
        val ec1Selector = factors(ec1Index * 2)
        val ec1Result = factors(ec1Index * 2 + 1)
        val ec2Selector = factors(ec2Index * 2)
        val ec2Result = factors(ec2Index * 2 + 1)
        val vtIndex1T = tupleVar.range.indexOf(Regular(List(Regular(ec1), Regular(true))))
        val vtIndex1F = tupleVar.range.indexOf(Regular(List(Regular(ec1), Regular(false))))
        val vtIndex2T = tupleVar.range.indexOf(Regular(List(Regular(ec2), Regular(true))))
        val vtIndex2F = tupleVar.range.indexOf(Regular(List(Regular(ec2), Regular(false))))
        ec1Selector.variables.size should equal (2)
        ec1Selector.variables(0) should equal (tupleVar)
        ec1Selector.variables(1).range should equal (List(Regular(true)))
        ec1Selector.size should equal (4)
        ec1Selector.get(List(vtIndex1T, 0)) should equal (1.0)
        ec1Selector.get(List(vtIndex1F, 0)) should equal (0.0)
        ec1Selector.get(List(vtIndex2T, 0)) should equal (1.0)
        ec1Selector.get(List(vtIndex2F, 0)) should equal (1.0)

        ec1Result.variables.size should equal (2)
        ec1Result.variables(0) should equal (c11.variable)
        ec1Result.variables(1).range should equal (List(Regular(true)))
        ec1Result.size should equal (1)
        ec1Result.get(List(0, 0)) should equal (1.0)
      }
    }

    "given an three-step single-valued reference with all required elements added and reference uncertainty without *" should {
      "produce the required conditional selector and result factors" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val ec3 = new EC1(universe)
        val ec4 = new EC1(universe)
        val e11 = Constant(true)("e1", ec1)
        val e12 = Constant(false)("e1", ec2)
        val e23 = Uniform(ec1, ec2)("e2", ec3)
        val e24 = Constant(ec1)("e2", ec4)
        val e3 = Uniform(ec3, ec4)("e3", universe)
        val e4 = universe.get[Boolean]("e3.e2.e1")
        pr.add(e11)
        pr.add(e12)
        pr.add(e23)
        pr.add(e24)
        pr.add(e3)
        pr.add(e4)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c23 = cc(e23)
        val c24 = cc(e24)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c11.generateRange()
        c12.generateRange()
        c23.generateRange()
        c24.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        // Eight factors should be produced:
        // Two conditional selectors at the top level,
        // Three conditional selectors at the second level
        // Three result factors at the third level
        val topTupleFactor :: factors = c4.nonConstraintFactors
        val topTupleVar = topTupleFactor.variables(2)
        val topIndex3T = topTupleVar.range.indexOf(Regular(List(Regular(ec3), Regular(true))))
        val topIndex3F = topTupleVar.range.indexOf(Regular(List(Regular(ec3), Regular(false))))
        val topIndex4T = topTupleVar.range.indexOf(Regular(List(Regular(ec4), Regular(true))))
        val topIndex4F = topTupleVar.range.indexOf(Regular(List(Regular(ec4), Regular(false))))
        val c23Index1 = c23.variable.range.indexOf(Regular(ec1))
        val c23Index2 = c23.variable.range.indexOf(Regular(ec2))
        val c24Index2 = c24.variable.range.indexOf(Regular(ec2))
        val c3Index3 = c3.variable.range.indexOf(Regular(ec3))
        val c3Index4 = c3.variable.range.indexOf(Regular(ec4))
        val c4IndexTrue = c4.variable.range.indexOf(Regular(true))
        val c4IndexFalse = c4.variable.range.indexOf(Regular(false))

        val (ec3Factors, ec4Factors) =
          if (c3Index3 == 0) (factors.slice(0, 6), factors.slice(6, 10)) else (factors.slice(4, 10), factors.slice(0, 4))
        val ec3Selector :: ec3TupleFactor :: ec3RestFactors = ec3Factors
        val (ec31Selector, ec31Result, ec32Selector, ec32Result) =
          if (c23Index1 == 0) (ec3RestFactors(0), ec3RestFactors(1), ec3RestFactors(2), ec3RestFactors(3))
          else (ec3RestFactors(2), ec3RestFactors(3), ec3RestFactors(0), ec3RestFactors(1))
        val List(ec4Selector, ec4TupleFactor, ec41Selector, ec41Result) = ec4Factors

        ec3Selector.variables.size should equal (2)
        ec3Selector.variables(0) should equal (topTupleVar)
        val ec3Var = ec3Selector.variables(1)
        ec3Var.range should equal (List(Regular(true), Regular(false)))
        val ec3VarIndexTrue = ec3Var.range.indexOf(Regular(true))
        val ec3VarIndexFalse = ec3Var.range.indexOf(Regular(false))
        ec3Selector.size should equal (8)
        ec3Selector.get(List(topIndex3T, c4IndexTrue)) should equal (1.0)
        ec3Selector.get(List(topIndex3T, c4IndexFalse)) should equal (0.0)
        ec3Selector.get(List(topIndex3F, c4IndexTrue)) should equal (0.0)
        ec3Selector.get(List(topIndex3F, c4IndexFalse)) should equal (1.0)
        ec3Selector.get(List(topIndex4T, c4IndexTrue)) should equal (1.0)
        ec3Selector.get(List(topIndex4T, c4IndexFalse)) should equal (1.0)
        ec3Selector.get(List(topIndex4F, c4IndexTrue)) should equal (1.0)
        ec3Selector.get(List(topIndex4F, c4IndexFalse)) should equal (1.0)

        val ec3TupleVar = ec3TupleFactor.variables(2)
        val ec3Index1T = ec3TupleVar.range.indexOf(Regular(List(Regular(ec1), Regular(true))))
        val ec3Index1F = ec3TupleVar.range.indexOf(Regular(List(Regular(ec1), Regular(false))))
        val ec3Index2T = ec3TupleVar.range.indexOf(Regular(List(Regular(ec2), Regular(true))))
        val ec3Index2F = ec3TupleVar.range.indexOf(Regular(List(Regular(ec2), Regular(false))))
        ec31Selector.variables.size should equal (2)
        ec31Selector.variables(0) should equal (ec3TupleVar)
        val ec31Var = ec31Selector.variables(1)
        ec31Var.range should equal (List(Regular(true)))
        ec31Selector.size should equal (4)
        ec31Selector.get(List(ec3Index1T, 0)) should equal (1.0)
        ec31Selector.get(List(ec3Index1F, 0)) should equal (0.0)
        ec31Selector.get(List(ec3Index2T, 0)) should equal (1.0)
        ec31Selector.get(List(ec3Index2F, 0)) should equal (1.0)

        ec31Result.variables should equal (List(c11.variable, ec31Var))
        ec31Result.size should equal (1)
        ec31Result.get(List(0, 0)) should equal (1.0)
      }
    }

    "given an indirect single-valued reference with all required elements added and reference uncertainty with *" should {
      "produce the conditional selector and result factors for the regular values and the star conditional selector" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e11 = Constant(true)("e1", ec1)
        val e12 = Constant(false)("e1", ec2)
        val e21 = Constant(ec1)
        val e22 = Constant(ec2)
        val e3 = Uniform(e21, e22)("e3", universe)
        val e4 = universe.get[Boolean]("e3.e1")
        pr.add(e11)
        pr.add(e12)
        pr.add(e22)
        pr.add(e3)
        pr.add(e4)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c22 = cc(e22)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c11.generateRange()
        c12.generateRange()
        c22.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        // Three factors should be produced: one regular conditional selector with its possibility, and one * conditional selector
        // The conditional selector and result factors for each parent value are produced in turn
        val e3ec2Index = c3.variable.range.indexOf(Regular(ec2))
        val e3StarIndex = c3.variable.range.indexWhere(!_.isRegular)
        val e3Star = c3.variable.range(e3StarIndex)
        val e4FalseIndex = c4.variable.range.indexOf(Regular(false))
        val e4StarIndex = c4.variable.range.indexWhere(!_.isRegular)
        val e4Star = c4.variable.range(e4StarIndex)
        val tupleFactor :: factors = c4.nonConstraintFactors
        val tupleVar = tupleFactor.variables(2)
        val vtIndexStarStar = tupleVar.range.indexOf(Regular(List(e3Star, e4Star)))
        val vtIndexStarF = tupleVar.range.indexOf(Regular(List(e3Star, Regular(false))))
        val vtIndex2Star = tupleVar.range.indexOf(Regular(List(Regular(ec2), e4Star)))
        val vtIndex2F = tupleVar.range.indexOf(Regular(List(Regular(ec2), Regular(false))))

        factors.size should equal (3)
        val (ec2Selector, ec2Result, starSelector) =
          if (e4FalseIndex == 0) (factors(0), factors(1), factors(2)) else (factors(1), factors(2), factors(0))

        ec2Selector.variables.size should equal (2)
        ec2Selector.variables(0) should equal (tupleVar)
        ec2Selector.variables(1).range should equal (List(Regular(false)))
        ec2Selector.size should equal (4)
        ec2Selector.get(List(vtIndex2Star, 0)) should equal (0.0)
        ec2Selector.get(List(vtIndex2F, 0)) should equal (1.0)
        ec2Selector.get(List(vtIndexStarStar, 0)) should equal (1.0)
        ec2Selector.get(List(vtIndexStarF, 0)) should equal (1.0)

        ec2Result.variables.size should equal (2)
        ec2Result.variables(0) should equal (c12.variable)
        ec2Result.variables(1).range should equal (List(Regular(false)))
        ec2Result.size should equal (1)
        ec2Result.get(List(0, 0)) should equal (1.0)

        starSelector.variables.size should equal (2)
        starSelector.variables(0) should equal (tupleVar)
        starSelector.variables(1).range.size should equal (1)
        starSelector.variables(1).range(0).isRegular should equal (false)
        starSelector.size should equal (4)
        starSelector.get(List(vtIndex2Star, 0)) should equal (1.0)
        starSelector.get(List(vtIndex2F, 0)) should equal (1.0)
        starSelector.get(List(vtIndexStarStar, 0)) should equal (1.0)
        starSelector.get(List(vtIndexStarF, 0)) should equal (0.0)
      }
    }

    "for a simple multi-valued reference y without *" should {
      "produce the single factor mapping single values to singleton multisets" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Boolean]) = ms(true)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Flip(0.5)("e1", universe)
        val e2 = universe.getAggregate[Boolean, Int](aggregate)("e1")
        val e3 = e2.mvre
        pr.add(e1)
        pr.add(e3)
        val c1 = cc(e1)
        val c3 = cc(e3)
        c1.generateRange()
        c3.generateRange()
        c3.makeNonConstraintFactors()

        val List(factor) = c3.nonConstraintFactors
        factor.variables should equal (List(c1.variable, c3.variable))
        factor.contents.size should equal (2)
        val c1TrueIndex = c1.variable.range.indexOf(Regular(true))
        val c3TrueIndex = c3.variable.range.indexOf(Regular(HashMultiSet(true)))
        factor.get(List(c1TrueIndex, c3TrueIndex)) should equal (1.0)
        factor.get(List(1 - c1TrueIndex, 1 - c3TrueIndex)) should equal (1.0)
      }
    }

    "for a simple multi-valued reference y with *" should {
      "produce the single factor mapping regular values to singleton multisets and * to *" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Boolean]) = ms(true)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Constant(true)
        val e2 = Constant(false)
        val e3 = Uniform(e1, e2)("e3", universe)
        val e4 = universe.getAggregate[Boolean, Int](aggregate)("e3")
        val e5 = e4.mvre
        pr.add(e2)
        pr.add(e3)
        pr.add(e5)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c5 = cc(e5)
        c2.generateRange()
        c3.generateRange()
        c5.generateRange()
        c5.makeNonConstraintFactors()

        val List(factor) = c5.nonConstraintFactors
        factor.variables should equal (List(c3.variable, c5.variable))
        factor.contents.size should equal (2)
        val c3FalseIndex = c3.variable.range.indexOf(Regular(false))
        val c5FalseIndex = c5.variable.range.indexOf(Regular(HashMultiSet(false)))
        val c3StarIndex = c3.variable.range.indexWhere(!_.isRegular)
        val c5StarIndex = c5.variable.range.indexWhere(!_.isRegular)
        factor.get(List(c3FalseIndex, c5FalseIndex)) should equal (1.0)
        factor.get(List(c3StarIndex, c5StarIndex)) should equal (1.0)
      }
    }

    "given an indirect multi-valued reference with reference uncertainty and multiple values without *" should {
      "produce the tuple factor, conditional selectors and result factors for the values, as well as the inject and apply factors" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Int]) = ms(2)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e11 = Uniform(1, 2)("e1", ec1)
        val e12 = Uniform(2, 3)("e1", ec2)
        val e2 = Uniform(List(ec1), List(ec1, ec2))("e2", universe)
        val e3 = universe.getAggregate[Int, Int](aggregate)("e2.e1")
        val e4 = e3.mvre
        pr.add(e11)
        pr.add(e12)
        pr.add(e2)
        pr.add(e4)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        val c4 = cc(e4)
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val c11Index1 = c11.variable.range.indexOf(Regular(1))
        val c11Index2 = c11.variable.range.indexOf(Regular(2))
        val c12Index2 = c12.variable.range.indexOf(Regular(2))
        val c12Index3 = c12.variable.range.indexOf(Regular(3))
        val c2Index1 = c2.variable.range.indexOf(Regular(List(ec1)))
        val c2Index2 = c2.variable.range.indexOf(Regular(List(ec1, ec2)))
        val c4Index1 = c4.variable.range.indexOf(Regular(HashMultiSet(1)))
        val c4Index2 = c4.variable.range.indexOf(Regular(HashMultiSet(2)))
        val c4Index12 = c4.variable.range.indexOf(Regular(HashMultiSet(1, 2)))
        val c4Index13 = c4.variable.range.indexOf(Regular(HashMultiSet(1, 3)))
        val c4Index22 = c4.variable.range.indexOf(Regular(HashMultiSet(2, 2)))
        val c4Index23 = c4.variable.range.indexOf(Regular(HashMultiSet(2, 3)))

        // 10 factors should be produced: one tuple factor, two conditional selectors, 2 applys, 2 injects, and three for the simple references
        val tupleFactor :: factors = c4.nonConstraintFactors
        factors.size should equal (9)
        tupleFactor.variables(0) should equal (c2.variable)
        tupleFactor.variables(1) should equal (c4.variable)
        val tupleVar = tupleFactor.variables(2)
        val tvIndex11 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(1)))))
        val tvIndex12 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(2)))))
        val tvIndex112 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(1, 2)))))
        val tvIndex113 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(1, 3)))))
        val tvIndex122 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(2, 2)))))
        val tvIndex123 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(2, 3)))))
        val tvIndex21 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(1)))))
        val tvIndex22 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(2)))))
        val tvIndex212 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(1, 2)))))
        val tvIndex213 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(1, 3)))))
        val tvIndex222 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(2, 2)))))
        val tvIndex223 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(2, 3)))))

        val List(oneECSelector, oneECApply, oneECInject, oneECBasic1) =
          if (c2Index1 == 0) factors.slice(0, 4) else factors.slice(5, 9)
        val List(twoECSelector, twoECApply, twoECInject, twoECBasic1, twoECBasic2) =
          if (c2Index1 == 0) factors.slice(4, 9) else factors.slice(0, 5)

        oneECBasic1.variables.size should equal (2)
        oneECBasic1.variables(0) should equal (c11.variable)
        val oneECRestVar1 = oneECBasic1.variables(1)
        val oneRest1Index1 = oneECRestVar1.range.indexOf(Regular(HashMultiSet(1)))
        val oneRest1Index2 = oneECRestVar1.range.indexOf(Regular(HashMultiSet(2)))
        oneECBasic1.contents.size should equal (2)
        oneECBasic1.get(List(c11Index1, oneRest1Index1)) should equal (1.0)
        oneECBasic1.get(List(c11Index2, oneRest1Index2)) should equal (1.0)

        twoECBasic1.variables.size should equal (2)
        twoECBasic1.variables(0) should equal (c11.variable)
        val twoECRestVar1 = twoECBasic1.variables(1)
        val twoRest1Index1 = twoECRestVar1.range.indexOf(Regular(HashMultiSet(1)))
        val twoRest1Index2 = twoECRestVar1.range.indexOf(Regular(HashMultiSet(2)))
        twoECBasic1.contents.size should equal (2)
        twoECBasic1.get(List(c11Index1, twoRest1Index1)) should equal (1.0)
        twoECBasic1.get(List(c11Index2, twoRest1Index2)) should equal (1.0)

        twoECBasic2.variables.size should equal (2)
        twoECBasic2.variables(0) should equal (c12.variable)
        val twoECRestVar2 = twoECBasic2.variables(1)
        val twoRest2Index2 = twoECRestVar2.range.indexOf(Regular(HashMultiSet(2)))
        val twoRest2Index3 = twoECRestVar2.range.indexOf(Regular(HashMultiSet(3)))
        twoECBasic2.contents.size should equal (2)
        twoECBasic2.get(List(c12Index2, twoRest2Index2)) should equal (1.0)
        twoECBasic2.get(List(c12Index3, twoRest2Index3)) should equal (1.0)

        oneECInject.variables.size should equal (2)
        oneECInject.variables(0) should equal (oneECRestVar1)
        val oneECInjectVar = oneECInject.variables(1)
        val oneInjectIndex1 = oneECInjectVar.range.indexOf(Regular(List(HashMultiSet(1))))
        val oneInjectIndex2 = oneECInjectVar.range.indexOf(Regular(List(HashMultiSet(2))))
        oneECInject.get(List(oneRest1Index1, oneInjectIndex1)) should equal (1.0)
        oneECInject.get(List(oneRest1Index1, oneInjectIndex2)) should equal (0.0)
        oneECInject.get(List(oneRest1Index2, oneInjectIndex1)) should equal (0.0)
        oneECInject.get(List(oneRest1Index2, oneInjectIndex2)) should equal (1.0)

        twoECInject.variables.size should equal (3)
        twoECInject.variables(0) should equal (twoECRestVar1)
        twoECInject.variables(1) should equal (twoECRestVar2)
        val twoECInjectVar = twoECInject.variables(2)
        val twoInjectIndex12 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(1), HashMultiSet(2))))
        val twoInjectIndex13 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(1), HashMultiSet(3))))
        val twoInjectIndex22 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(2), HashMultiSet(2))))
        val twoInjectIndex23 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(2), HashMultiSet(3))))
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex12)) should equal (1.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex13)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex22)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex23)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex12)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex13)) should equal (1.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex22)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex23)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex12)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex13)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex22)) should equal (1.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex23)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex12)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex13)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex22)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex23)) should equal (1.0)

        oneECApply.variables.size should equal (2)
        oneECApply.variables(0) should equal (oneECInjectVar)
        val oneECApplyVar = oneECApply.variables(1)
        val oneApplyIndex1 = oneECApplyVar.range.indexOf(Regular(HashMultiSet(1)))
        val oneApplyIndex2 = oneECApplyVar.range.indexOf(Regular(HashMultiSet(2)))
        oneECApply.contents.size should equal (2)
        oneECApply.get(List(oneInjectIndex1, oneApplyIndex1)) should equal (1.0)
        oneECApply.get(List(oneInjectIndex2, oneApplyIndex2)) should equal (1.0)

        twoECApply.variables.size should equal (2)
        twoECApply.variables(0) should equal (twoECInjectVar)
        val twoECApplyVar = twoECApply.variables(1)
        val twoApplyIndex12 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(1, 2)))
        val twoApplyIndex13 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(1, 3)))
        val twoApplyIndex22 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(2, 2)))
        val twoApplyIndex23 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(2, 3)))
        twoECApply.contents.size should equal (4)
        twoECApply.get(List(twoInjectIndex12, twoApplyIndex12)) should equal (1.0)
        twoECApply.get(List(twoInjectIndex13, twoApplyIndex13)) should equal (1.0)
        twoECApply.get(List(twoInjectIndex22, twoApplyIndex22)) should equal (1.0)
        twoECApply.get(List(twoInjectIndex23, twoApplyIndex23)) should equal (1.0)

        oneECSelector.variables should equal (List(tupleVar, oneECApplyVar))
        oneECSelector.get(List(tvIndex11, oneApplyIndex1)) should equal (1.0)
        oneECSelector.get(List(tvIndex11, oneApplyIndex2)) should equal (0.0)
        oneECSelector.get(List(tvIndex21, oneApplyIndex2)) should equal (1.0)

        twoECSelector.variables should equal (List(tupleVar, twoECApplyVar))
        twoECSelector.get(List(tvIndex212, twoApplyIndex12)) should equal (1.0)
        twoECSelector.get(List(tvIndex212, twoApplyIndex13)) should equal (0.0)
        twoECSelector.get(List(tvIndex112, twoApplyIndex13)) should equal (1.0)
      }
    }

    "given an three-step multi-valued reference with reference uncertainty and multiple values without *" should {
      "produce the tuple factors, conditional selectors and result factors for the values, as well as the inject and apply factors" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Int]) = ms(2)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val ec3 = new EC1(universe)
        val ec4 = new EC1(universe)
        val e11 = Uniform(1, 2)("e1", ec1)
        val e12 = Uniform(2, 3)("e1", ec2)
        val e23 = Uniform(List(ec1), List(ec1, ec2))("e2", ec3)
        val e24 = Constant(List(ec1))("e2", ec4)
        val e3 = Uniform(ec3, ec4)("e3", universe)
        val e4 = universe.getAggregate[Int, Int](aggregate)("e3.e2.e1")
        val e5 = e4.mvre
        pr.add(e11)
        pr.add(e12)
        pr.add(e23)
        pr.add(e24)
        pr.add(e3)
        pr.add(e5)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c23 = cc(e23)
        val c24 = cc(e24)
        val c3 = cc(e3)
        val c5 = cc(e5)
        c11.generateRange()
        c12.generateRange()
        c23.generateRange()
        c24.generateRange()
        c3.generateRange()
        c5.generateRange()
        c5.makeNonConstraintFactors()

        // 18 factors are produced
        // 1 top level tuple factor, 2 top level selectors
        // - for option ec3: 1 tuple factor, 2 selectors, 2 applys, 2 injects, 3 results
        // - for option ec4: 1 tuple factor, 1 selector, 1 apply, 1 inject, 1 result
        val topTupleFactor :: factors = c5.nonConstraintFactors
        factors.size should equal (17)
        val c11Index1 = c11.variable.range.indexOf(Regular(1))
        val c11Index2 = c11.variable.range.indexOf(Regular(2))
        val c12Index2 = c12.variable.range.indexOf(Regular(2))
        val c12Index3 = c12.variable.range.indexOf(Regular(3))
        val c23Index1 = c23.variable.range.indexOf(Regular(List(ec1)))
        val c23Index12 = c23.variable.range.indexOf(Regular(List(ec1, ec2)))
        val c24Index1 = c24.variable.range.indexOf(Regular(List(ec1)))
        val c3Index3 = c3.variable.range.indexOf(Regular(ec3))
        val c3Index4 = c3.variable.range.indexOf(Regular(ec4))
        val c5Index1 = c5.variable.range.indexOf(Regular(HashMultiSet(1)))
        val c5Index2 = c5.variable.range.indexOf(Regular(HashMultiSet(2)))
        val c5Index12 = c5.variable.range.indexOf(Regular(HashMultiSet(1, 2)))
        val c5Index13 = c5.variable.range.indexOf(Regular(HashMultiSet(1, 3)))
        val c5Index22 = c5.variable.range.indexOf(Regular(HashMultiSet(2, 2)))
        val c5Index23 = c5.variable.range.indexOf(Regular(HashMultiSet(2, 3)))

        val (ec3Factors, ec4Factors) =
          if (c3Index3 == 0) (factors.slice(0, 11), factors.slice(11, 17)) else (factors.slice(0, 6), factors.slice(6, 17))
        val ec3Selector :: ec3TupleFactor :: ec3Tail = ec3Factors
        val (ec31Factors, ec312Factors) =
          if (c23Index1 == 0) (ec3Tail.slice(0, 4), ec3Tail.slice(4, 9)) else (ec3Tail.slice(5, 9), ec3Tail.slice(0, 5))
        val List(ec31Selector, ec31Apply, ec31Inject, ec31Result1) = ec31Factors
        val List(ec312Selector, ec312Apply, ec312Inject, ec312Result1, ec312Result2) = ec312Factors
        val List(ec4Selector, ec4TupleFactor, ec41Selector, ec41Apply, ec41Inject, ec41Result1) = ec4Factors

        ec31Result1.variables.size should equal (2)
        ec31Result1.variables(0) should equal (c11.variable)
        val ec31Result1Var = ec31Result1.variables(1)
        val ec31Result1Index1 = ec31Result1Var.range.indexOf(Regular(HashMultiSet(1)))
        val ec31Result1Index2 = ec31Result1Var.range.indexOf(Regular(HashMultiSet(2)))
        ec31Result1.contents.size should equal (2)
        ec31Result1.get(List(c11Index1, ec31Result1Index1)) should equal (1.0)
        ec31Result1.get(List(c11Index2, ec31Result1Index2)) should equal (1.0)

        ec312Result1.variables.size should equal (2)
        ec312Result1.variables(0) should equal (c11.variable)
        val ec312Result1Var = ec312Result1.variables(1)
        val ec312Result1Index1 = ec312Result1Var.range.indexOf(Regular(HashMultiSet(1)))
        val ec312Result1Index2 = ec312Result1Var.range.indexOf(Regular(HashMultiSet(2)))
        ec312Result1.contents.size should equal (2)
        ec312Result1.get(List(c11Index1, ec312Result1Index1)) should equal (1.0)
        ec312Result1.get(List(c11Index2, ec312Result1Index2)) should equal (1.0)

        ec312Result2.variables.size should equal (2)
        ec312Result2.variables(0) should equal (c12.variable)
        val ec312Result2Var = ec312Result2.variables(1)
        val ec312Result2Index2 = ec312Result2Var.range.indexOf(Regular(HashMultiSet(2)))
        val ec312Result2Index3 = ec312Result2Var.range.indexOf(Regular(HashMultiSet(3)))
        ec312Result2.contents.size should equal (2)
        ec312Result2.get(List(c12Index2, ec312Result2Index2)) should equal (1.0)
        ec312Result2.get(List(c12Index3, ec312Result2Index3)) should equal (1.0)

        ec41Result1.variables.size should equal (2)
        ec41Result1.variables(0) should equal (c11.variable)
        val ec41Result1Var = ec41Result1.variables(1)
        val ec41Result1Index1 = ec41Result1Var.range.indexOf(Regular(HashMultiSet(1)))
        val ec41Result1Index2 = ec41Result1Var.range.indexOf(Regular(HashMultiSet(2)))
        ec41Result1.contents.size should equal (2)
        ec41Result1.get(List(c11Index1, ec41Result1Index1)) should equal (1.0)
        ec41Result1.get(List(c11Index2, ec41Result1Index2)) should equal (1.0)

        ec31Inject.variables.size should equal (2)
        ec31Inject.variables(0) should equal (ec31Result1Var)
        val ec31InjectVar = ec31Inject.variables(1)
        val ec31InjectIndex1 = ec31InjectVar.range.indexOf(Regular(List(HashMultiSet(1))))
        val ec31InjectIndex2 = ec31InjectVar.range.indexOf(Regular(List(HashMultiSet(2))))
        ec31Inject.get(List(ec31Result1Index1, ec31InjectIndex1)) should equal (1.0)
        ec31Inject.get(List(ec31Result1Index1, ec31InjectIndex2)) should equal (0.0)
        ec31Inject.get(List(ec31Result1Index2, ec31InjectIndex1)) should equal (0.0)
        ec31Inject.get(List(ec31Result1Index2, ec31InjectIndex2)) should equal (1.0)

        ec312Inject.variables.size should equal (3)
        ec312Inject.variables(0) should equal (ec312Result1Var)
        ec312Inject.variables(1) should equal (ec312Result2Var)
        val ec312InjectVar = ec312Inject.variables(2)
        val ec312InjectIndex12 = ec312InjectVar.range.indexOf(Regular(List(HashMultiSet(1), HashMultiSet(2))))
        val ec312InjectIndex13 = ec312InjectVar.range.indexOf(Regular(List(HashMultiSet(1), HashMultiSet(3))))
        val ec312InjectIndex22 = ec312InjectVar.range.indexOf(Regular(List(HashMultiSet(2), HashMultiSet(2))))
        val ec312InjectIndex23 = ec312InjectVar.range.indexOf(Regular(List(HashMultiSet(2), HashMultiSet(3))))
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index2, ec312InjectIndex12)) should equal (1.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index2, ec312InjectIndex13)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index2, ec312InjectIndex22)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index2, ec312InjectIndex23)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index3, ec312InjectIndex12)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index3, ec312InjectIndex13)) should equal (1.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index3, ec312InjectIndex22)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index1, ec312Result2Index3, ec312InjectIndex23)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index2, ec312InjectIndex12)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index2, ec312InjectIndex13)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index2, ec312InjectIndex22)) should equal (1.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index2, ec312InjectIndex23)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index3, ec312InjectIndex12)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index3, ec312InjectIndex13)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index3, ec312InjectIndex22)) should equal (0.0)
        ec312Inject.get(List(ec312Result1Index2, ec312Result2Index3, ec312InjectIndex23)) should equal (1.0)

        ec41Inject.variables.size should equal (2)
        ec41Inject.variables(0) should equal (ec41Result1Var)
        val ec41InjectVar = ec41Inject.variables(1)
        val ec41InjectIndex1 = ec41InjectVar.range.indexOf(Regular(List(HashMultiSet(1))))
        val ec41InjectIndex2 = ec41InjectVar.range.indexOf(Regular(List(HashMultiSet(2))))
        ec41Inject.get(List(ec41Result1Index1, ec41InjectIndex1)) should equal (1.0)
        ec41Inject.get(List(ec41Result1Index1, ec41InjectIndex2)) should equal (0.0)
        ec41Inject.get(List(ec41Result1Index2, ec41InjectIndex1)) should equal (0.0)
        ec41Inject.get(List(ec41Result1Index2, ec41InjectIndex2)) should equal (1.0)

        ec31Apply.variables.size should equal (2)
        ec31Apply.variables(0) should equal (ec31InjectVar)
        val ec31ApplyVar = ec31Apply.variables(1)
        val ec31ApplyIndex1 = ec31ApplyVar.range.indexOf(Regular(HashMultiSet(1)))
        val ec31ApplyIndex2 = ec31ApplyVar.range.indexOf(Regular(HashMultiSet(2)))
        ec31Apply.contents.size should equal (2)
        ec31Apply.get(List(ec31InjectIndex1, ec31ApplyIndex1)) should equal (1.0)
        ec31Apply.get(List(ec31InjectIndex2, ec31ApplyIndex2)) should equal (1.0)

        ec312Apply.variables.size should equal (2)
        ec312Apply.variables(0) should equal (ec312InjectVar)
        val ec312ApplyVar = ec312Apply.variables(1)
        val ec312ApplyIndex12 = ec312ApplyVar.range.indexOf(Regular(HashMultiSet(1, 2)))
        val ec312ApplyIndex13 = ec312ApplyVar.range.indexOf(Regular(HashMultiSet(1, 3)))
        val ec312ApplyIndex22 = ec312ApplyVar.range.indexOf(Regular(HashMultiSet(2, 2)))
        val ec312ApplyIndex23 = ec312ApplyVar.range.indexOf(Regular(HashMultiSet(2, 3)))
        ec312Apply.contents.size should equal (4)
        ec312Apply.get(List(ec312InjectIndex12, ec312ApplyIndex12)) should equal (1.0)
        ec312Apply.get(List(ec312InjectIndex13, ec312ApplyIndex13)) should equal (1.0)
        ec312Apply.get(List(ec312InjectIndex22, ec312ApplyIndex22)) should equal (1.0)
        ec312Apply.get(List(ec312InjectIndex23, ec312ApplyIndex23)) should equal (1.0)

        ec41Apply.variables.size should equal (2)
        ec41Apply.variables(0) should equal (ec41InjectVar)
        val ec41ApplyVar = ec41Apply.variables(1)
        val ec41ApplyIndex1 = ec41ApplyVar.range.indexOf(Regular(HashMultiSet(1)))
        val ec41ApplyIndex2 = ec41ApplyVar.range.indexOf(Regular(HashMultiSet(2)))
        ec41Apply.contents.size should equal (2)
        ec41Apply.get(List(ec41InjectIndex1, ec41ApplyIndex1)) should equal (1.0)
        ec41Apply.get(List(ec41InjectIndex2, ec41ApplyIndex2)) should equal (1.0)


        val topTupleVar = topTupleFactor.variables(2)
        val ec3TupleVar = ec3TupleFactor.variables(2)
        val ec4TupleVar = ec4TupleFactor.variables(2)

        ec31Selector.variables should equal (List(ec3TupleVar, ec31ApplyVar))
        ec312Selector.variables should equal (List(ec3TupleVar, ec312ApplyVar))
        ec41Selector.variables should equal (List(ec4TupleVar, ec41ApplyVar))
        ec3Selector.variables.size should equal (2)
        ec3Selector.variables(0) should equal (topTupleVar)
        ec4Selector.variables.size should equal (2)
        ec4Selector.variables(0) should equal (topTupleVar)
      }
    }

    "given an indirect multi-valued reference with reference uncertainty and multiple values with *" should {
      "produce the tuple factor, conditional selectors and result factors for the values, as well as the inject and apply factors, plus the * selector" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Int]) = ms(2)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e11 = Uniform(1, 2)("e1", ec1)
        val e12 = Uniform(2, 3)("e1", ec2)
        val l1 = Constant(List(ec1))
        val l12 = Constant(List(ec1, ec2))
        val l2 = Constant(List(ec2))
        val e2 = Uniform(l1, l12, l2)("e2", universe)
        val e3 = universe.getAggregate[Int, Int](aggregate)("e2.e1")
        val e4 = e3.mvre
        pr.add(e11)
        pr.add(e12)
        pr.add(l1)
        pr.add(l12)
        pr.add(e2)
        pr.add(e4)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val cl1 = cc(l1)
        val cl12 = cc(l12)
        val c2 = cc(e2)
        val c4 = cc(e4)
        c11.generateRange()
        c12.generateRange()
        cl1.generateRange()
        cl12.generateRange()
        c2.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val c11Index1 = c11.variable.range.indexOf(Regular(1))
        val c11Index2 = c11.variable.range.indexOf(Regular(2))
        val c12Index2 = c12.variable.range.indexOf(Regular(2))
        val c12Index3 = c12.variable.range.indexOf(Regular(3))
        val c2Index1 = c2.variable.range.indexOf(Regular(List(ec1)))
        val c2Index12 = c2.variable.range.indexOf(Regular(List(ec1, ec2)))
        val c2IndexStar = c2.variable.range.indexWhere(!_.isRegular)
        val c2Star = c2.variable.range(c2IndexStar)
        val c4Index1 = c4.variable.range.indexOf(Regular(HashMultiSet(1)))
        val c4Index2 = c4.variable.range.indexOf(Regular(HashMultiSet(2)))
        val c4Index12 = c4.variable.range.indexOf(Regular(HashMultiSet(1, 2)))
        val c4Index13 = c4.variable.range.indexOf(Regular(HashMultiSet(1, 3)))
        val c4Index22 = c4.variable.range.indexOf(Regular(HashMultiSet(2, 2)))
        val c4Index23 = c4.variable.range.indexOf(Regular(HashMultiSet(2, 3)))
        val c4IndexStar = c4.variable.range.indexWhere(!_.isRegular)
        val c4Star = c4.variable.range(c4IndexStar)

        // 1 factors should be produced:
        // one tuple factor, two conditional selectors, one star selector, 2 applys, 2 injects, and three for the simple references
        val tupleFactor :: factors = c4.nonConstraintFactors
        factors.size should equal (10)
        val tupleVar = tupleFactor.variables(2)
        tupleFactor.variables(0) should equal (c2.variable)
        tupleFactor.variables(1) should equal (c4.variable)
        val tvIndex11 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(1)))))
        val tvIndex12 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(2)))))
        val tvIndex112 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(1, 2)))))
        val tvIndex113 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(1, 3)))))
        val tvIndex122 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(2, 2)))))
        val tvIndex123 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), Regular(HashMultiSet(2, 3)))))
        val tvIndex1Star = tupleVar.range.indexOf(Regular(List(Regular(List(ec1)), c4Star)))
        val tvIndex21 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(1)))))
        val tvIndex22 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(2)))))
        val tvIndex212 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(1, 2)))))
        val tvIndex213 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(1, 3)))))
        val tvIndex222 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(2, 2)))))
        val tvIndex223 = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), Regular(HashMultiSet(2, 3)))))
        val tvIndex2Star = tupleVar.range.indexOf(Regular(List(Regular(List(ec1, ec2)), c4Star)))
        val tvIndexStar1 = tupleVar.range.indexOf(Regular(List(c2Star, Regular(HashMultiSet(1)))))
        val tvIndexStar2 = tupleVar.range.indexOf(Regular(List(c2Star, Regular(HashMultiSet(2)))))
        val tvIndexStar12 = tupleVar.range.indexOf(Regular(List(c2Star, Regular(HashMultiSet(1, 2)))))
        val tvIndexStar13 = tupleVar.range.indexOf(Regular(List(c2Star, Regular(HashMultiSet(1, 3)))))
        val tvIndexStar22 = tupleVar.range.indexOf(Regular(List(c2Star, Regular(HashMultiSet(2, 2)))))
        val tvIndexStar23 = tupleVar.range.indexOf(Regular(List(c2Star, Regular(HashMultiSet(2, 3)))))
        val tvIndexStarStar = tupleVar.range.indexOf(Regular(List(c2Star, c4Star)))

        val (oneFactors, twoFactors, starSelector) =
          (c2Index1, c2Index12, c2IndexStar) match {
            case (0, 1, 2) => (factors.slice(0, 4), factors.slice(4, 9), factors(9))
            case (0, 2, 1) => (factors.slice(0, 4), factors.slice(9, 10), factors(5))
            case (1, 0, 2) => (factors.slice(5, 9), factors.slice(0, 5), factors(9))
            case (1, 2, 0) => (factors.slice(1, 5), factors.slice(5, 10), factors(0))
            case (2, 0, 1) => (factors.slice(6, 10), factors.slice(0, 5), factors(5))
            case (2, 1, 0) => (factors.slice(6, 10), factors.slice(2, 6), factors(0))
          }
        val List(oneECSelector, oneECApply, oneECInject, oneECBasic1) = oneFactors
        val List(twoECSelector, twoECApply, twoECInject, twoECBasic1, twoECBasic2) = twoFactors

        oneECBasic1.variables.size should equal (2)
        oneECBasic1.variables(0) should equal (c11.variable)
        val oneECRestVar1 = oneECBasic1.variables(1)
        val oneRest1Index1 = oneECRestVar1.range.indexOf(Regular(HashMultiSet(1)))
        val oneRest1Index2 = oneECRestVar1.range.indexOf(Regular(HashMultiSet(2)))
        oneECBasic1.contents.size should equal (2)
        oneECBasic1.get(List(c11Index1, oneRest1Index1)) should equal (1.0)
        oneECBasic1.get(List(c11Index2, oneRest1Index2)) should equal (1.0)

        twoECBasic1.variables.size should equal (2)
        twoECBasic1.variables(0) should equal (c11.variable)
        val twoECRestVar1 = twoECBasic1.variables(1)
        val twoRest1Index1 = twoECRestVar1.range.indexOf(Regular(HashMultiSet(1)))
        val twoRest1Index2 = twoECRestVar1.range.indexOf(Regular(HashMultiSet(2)))
        twoECBasic1.contents.size should equal (2)
        twoECBasic1.get(List(c11Index1, twoRest1Index1)) should equal (1.0)
        twoECBasic1.get(List(c11Index2, twoRest1Index2)) should equal (1.0)

        twoECBasic2.variables.size should equal (2)
        twoECBasic2.variables(0) should equal (c12.variable)
        val twoECRestVar2 = twoECBasic2.variables(1)
        val twoRest2Index2 = twoECRestVar2.range.indexOf(Regular(HashMultiSet(2)))
        val twoRest2Index3 = twoECRestVar2.range.indexOf(Regular(HashMultiSet(3)))
        twoECBasic2.contents.size should equal (2)
        twoECBasic2.get(List(c12Index2, twoRest2Index2)) should equal (1.0)
        twoECBasic2.get(List(c12Index3, twoRest2Index3)) should equal (1.0)

        oneECInject.variables.size should equal (2)
        oneECInject.variables(0) should equal (oneECRestVar1)
        val oneECInjectVar = oneECInject.variables(1)
        val oneInjectIndex1 = oneECInjectVar.range.indexOf(Regular(List(HashMultiSet(1))))
        val oneInjectIndex2 = oneECInjectVar.range.indexOf(Regular(List(HashMultiSet(2))))
        oneECInject.get(List(oneRest1Index1, oneInjectIndex1)) should equal (1.0)
        oneECInject.get(List(oneRest1Index1, oneInjectIndex2)) should equal (0.0)
        oneECInject.get(List(oneRest1Index2, oneInjectIndex1)) should equal (0.0)
        oneECInject.get(List(oneRest1Index2, oneInjectIndex2)) should equal (1.0)

        twoECInject.variables.size should equal (3)
        twoECInject.variables(0) should equal (twoECRestVar1)
        twoECInject.variables(1) should equal (twoECRestVar2)
        val twoECInjectVar = twoECInject.variables(2)
        val twoInjectIndex12 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(1), HashMultiSet(2))))
        val twoInjectIndex13 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(1), HashMultiSet(3))))
        val twoInjectIndex22 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(2), HashMultiSet(2))))
        val twoInjectIndex23 = twoECInjectVar.range.indexOf(Regular(List(HashMultiSet(2), HashMultiSet(3))))
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex12)) should equal (1.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex13)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex22)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index2, twoInjectIndex23)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex12)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex13)) should equal (1.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex22)) should equal (0.0)
        twoECInject.get(List(twoRest1Index1, twoRest2Index3, twoInjectIndex23)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex12)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex13)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex22)) should equal (1.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index2, twoInjectIndex23)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex12)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex13)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex22)) should equal (0.0)
        twoECInject.get(List(twoRest1Index2, twoRest2Index3, twoInjectIndex23)) should equal (1.0)

        oneECApply.variables.size should equal (2)
        oneECApply.variables(0) should equal (oneECInjectVar)
        val oneECApplyVar = oneECApply.variables(1)
        val oneApplyIndex1 = oneECApplyVar.range.indexOf(Regular(HashMultiSet(1)))
        val oneApplyIndex2 = oneECApplyVar.range.indexOf(Regular(HashMultiSet(2)))
        oneECApply.contents.size should equal (2)
        oneECApply.get(List(oneInjectIndex1, oneApplyIndex1)) should equal (1.0)
        oneECApply.get(List(oneInjectIndex2, oneApplyIndex2)) should equal (1.0)

        twoECApply.variables.size should equal (2)
        twoECApply.variables(0) should equal (twoECInjectVar)
        val twoECApplyVar = twoECApply.variables(1)
        val twoApplyIndex12 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(1, 2)))
        val twoApplyIndex13 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(1, 3)))
        val twoApplyIndex22 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(2, 2)))
        val twoApplyIndex23 = twoECApplyVar.range.indexOf(Regular(HashMultiSet(2, 3)))
        twoECApply.contents.size should equal (4)
        twoECApply.get(List(twoInjectIndex12, twoApplyIndex12)) should equal (1.0)
        twoECApply.get(List(twoInjectIndex13, twoApplyIndex13)) should equal (1.0)
        twoECApply.get(List(twoInjectIndex22, twoApplyIndex22)) should equal (1.0)
        twoECApply.get(List(twoInjectIndex23, twoApplyIndex23)) should equal (1.0)

        starSelector.variables.size should equal (2)
        starSelector.variables(0) should equal (tupleVar)
        starSelector.variables(1).valueSet.hasStar should equal (true)
        starSelector.variables(1).valueSet.regularValues should equal (Set())
        starSelector.get(List(tvIndexStarStar, 0)) should equal (1.0)
        starSelector.get(List(tvIndexStar1, 0)) should equal (0.0)
        starSelector.get(List(tvIndex21, 0)) should equal (1.0)
        oneECSelector.variables should equal (List(tupleVar, oneECApplyVar))
        oneECSelector.get(List(tvIndex11, oneApplyIndex1)) should equal (1.0)
        oneECSelector.get(List(tvIndex1Star, oneApplyIndex1)) should equal (0.0)
        oneECSelector.get(List(tvIndex2Star, oneApplyIndex1)) should equal (1.0)

        twoECSelector.variables should equal (List(tupleVar, twoECApplyVar))
        twoECSelector.get(List(tvIndex212, twoApplyIndex12)) should equal (1.0)
        twoECSelector.get(List(tvIndex2Star, twoApplyIndex12)) should equal (0.0)
        twoECSelector.get(List(tvIndex1Star, twoApplyIndex12)) should equal (1.0)
      }
    }

    "given an aggregate without *" should {
      "produce the factor that maps multisets to their aggregates" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Int]) = ms(2)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e11 = Uniform(1, 2)("e1", ec1)
        val e12 = Uniform(2, 3)("e1", ec2)
        val e2 = Uniform(List(ec1), List(ec1, ec2))("e2", universe)
        val e3 = universe.getAggregate[Int, Int](aggregate)("e2.e1")
        val e4 = e3.mvre
        pr.add(e11)
        pr.add(e12)
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c4.generateRange()
        c3.generateRange()
        c3.makeNonConstraintFactors()

        val List(factor) = c3.nonConstraintFactors
        factor.variables should equal (List(c4.variable, c3.variable))
        factor.contents.size should equal (6)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(1))), c3.variable.range.indexOf(Regular(0)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(2))), c3.variable.range.indexOf(Regular(1)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(1, 2))), c3.variable.range.indexOf(Regular(1)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(1, 3))), c3.variable.range.indexOf(Regular(0)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(2, 2))), c3.variable.range.indexOf(Regular(2)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(2, 3))), c3.variable.range.indexOf(Regular(1)))) should equal (1.0)
      }
    }

    "given an aggregate with *" should {
      "produce the factor that maps multisets to their aggregates and * to *" in {
        val universe = Universe.createNew()
        def aggregate(ms: MultiSet[Int]) = ms(2)
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e01 = Constant(1)
        val e02 = Constant(2)
        val e11 = Uniform(e01, e02)("e1", ec1)
        val e12 = Uniform(2, 3)("e1", ec2)
        val e2 = Uniform(List(ec1), List(ec1, ec2))("e2", universe)
        val e3 = universe.getAggregate[Int, Int](aggregate)("e2.e1")
        val e4 = e3.mvre
        pr.add(e02)
        pr.add(e11)
        pr.add(e12)
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        val c02 = cc(e02)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c02.generateRange()
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c4.generateRange()
        c3.generateRange()
        c3.makeNonConstraintFactors()

        val List(factor) = c3.nonConstraintFactors
        factor.variables should equal (List(c4.variable, c3.variable))
        factor.contents.size should equal (4)
        factor.get(List(c4.variable.range.indexWhere(!_.isRegular), c3.variable.range.indexWhere(!_.isRegular))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(2))), c3.variable.range.indexOf(Regular(1)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(2, 2))), c3.variable.range.indexOf(Regular(2)))) should equal (1.0)
        factor.get(List(c4.variable.range.indexOf(Regular(HashMultiSet(2, 3))), c3.variable.range.indexOf(Regular(1)))) should equal (1.0)
      }
    }

    "given a fully expanded MakeArray without *" should  {
      "produce the factor that maps numItems to the array of the appropriate length" in {
          val universe = Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val e1 = FromRange(0, 3)
          val e2 = new MakeArray("", e1, (i: Int) => Constant(i), universe)
          pr.add(e1)
          pr.add(e2)
          val c1 = cc(e1)
          val c2 = cc(e2)
          c1.generateRange()
          c2.expand()
          c2.generateRange()
          c2.makeNonConstraintFactors()

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c1.variable, c2.variable))
          factor.contents.size should equal (3)
          val c1Index0 = c1.variable.range.indexOf(Regular(0))
          val c1Index1 = c1.variable.range.indexOf(Regular(1))
          val c1Index2 = c1.variable.range.indexOf(Regular(2))
          val c2Index0 = c2.variable.range.indexWhere(_.value.size == 0)
          val c2Index1 = c2.variable.range.indexWhere(_.value.size == 1)
          val c2Index2 = c2.variable.range.indexWhere(_.value.size == 2)
          factor.get(List(c1Index0, c2Index0)) should equal (1.0)
          factor.get(List(c1Index1, c2Index1)) should equal (1.0)
          factor.get(List(c1Index2, c2Index2)) should equal (1.0)
      }
    }

    "given a fully expanded MakeArray with *" should  {
      "produce the factor that maps numItems to the array of the appropriate length and * to *" in {
          val universe = Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val e00 = Constant(0)
          val e01 = Constant(1)
          val e02 = Constant(2)
          val e1 = Uniform(e00, e01, e02)
          val e2 = new MakeArray("", e1, (i: Int) => Constant(i), universe)
          pr.add(e00)
          pr.add(e02)
          pr.add(e1)
          pr.add(e2)
          val c00 = cc(e00)
          val c02 = cc(e02)
          val c1 = cc(e1)
          val c2 = cc(e2)
          c00.generateRange()
          c02.generateRange()
          c1.generateRange()
          c2.expand()
          c2.generateRange()
          c2.makeNonConstraintFactors()

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c1.variable, c2.variable))
          factor.contents.size should equal (3)
          val c1Index0 = c1.variable.range.indexOf(Regular(0))
          val c1IndexStar = c1.variable.range.indexWhere(!_.isRegular)
          val c1Index2 = c1.variable.range.indexOf(Regular(2))
          val c2Index0 = c2.variable.range.indexWhere(_.value.size == 0)
          val c2IndexStar = c2.variable.range.indexWhere(!_.isRegular)
          val c2Index2 = c2.variable.range.indexWhere(_.value.size == 2)
          factor.get(List(c1Index0, c2Index0)) should equal (1.0)
          factor.get(List(c1IndexStar, c2IndexStar)) should equal (1.0)
          factor.get(List(c1Index2, c2Index2)) should equal (1.0)
      }
    }

    "given a partially expanded MakeArray without *" should  {
      "produce the factor that maps shorter numItems to the array of the appropriate length and longer numItems to *" in {
          val universe = Universe.createNew()
          val cc = new ComponentCollection
          val pr = new Problem(cc)
          val e1 = FromRange(0, 3)
          val e2 = new MakeArray("", e1, (i: Int) => Constant(i), universe)
          pr.add(e1)
          pr.add(e2)
          val c1 = cc(e1)
          val c2 = cc(e2)
          c1.generateRange()
          c2.expand(1)
          c2.generateRange()
          c2.makeNonConstraintFactors()

          val List(factor) = c2.nonConstraintFactors
          factor.variables should equal (List(c1.variable, c2.variable))
          factor.contents.size should equal (3)
          val c1Index0 = c1.variable.range.indexOf(Regular(0))
          val c1Index1 = c1.variable.range.indexOf(Regular(1))
          val c1Index2 = c1.variable.range.indexOf(Regular(2))
          val c2Index0 = c2.variable.range.indexWhere(_.value.size == 0)
          val c2Index1 = c2.variable.range.indexWhere(_.value.size == 1)
          val c2IndexStar = c2.variable.range.indexWhere(!_.isRegular)
          factor.get(List(c1Index0, c2Index0)) should equal (1.0)
          factor.get(List(c1Index1, c2Index1)) should equal (1.0)
          factor.get(List(c1Index2, c2IndexStar)) should equal (1.0)
      }
    }

    "given a fold without *" should {
      "produce the sequence of fold factors" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e1 = Flip(0.1)
        val e2 = Flip(0.2)
        val e3 = Flip(0.3)
        val e4 = FoldLeft(false, (b1: Boolean, b2: Boolean) => b1 || b2)(e1, e2, e3)
        pr.add(e1)
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val List(factor1, factor2, factor3) = c4.nonConstraintFactors
        factor1.variables.size should equal (3)
        val startVar = factor1.variables(0)
        factor1.variables(1) should equal (c1.variable)
        val accum1Var = factor1.variables(2)
        factor2.variables.size should equal (3)
        factor2.variables(0) should equal (accum1Var)
        factor2.variables(1) should equal (c2.variable)
        val accum2Var = factor2.variables(2)
        factor3.variables should equal (List(accum2Var, c3.variable, c4.variable))

        startVar.range should equal (List(Regular(false)))
        accum1Var.range.size should equal (2)
        accum1Var.range.contains(Regular(false)) should equal (true)
        accum1Var.range.contains(Regular(true)) should equal (true)
        accum2Var.range.size should equal (2)
        accum2Var.range.contains(Regular(false)) should equal (true)
        accum2Var.range.contains(Regular(true)) should equal (true)

        val c1IndexFalse = c1.variable.range.indexOf(Regular(false))
        val c1IndexTrue = c1.variable.range.indexOf(Regular(true))
        val c2IndexFalse = c2.variable.range.indexOf(Regular(false))
        val c2IndexTrue = c2.variable.range.indexOf(Regular(true))
        val c3IndexFalse = c3.variable.range.indexOf(Regular(false))
        val c3IndexTrue = c3.variable.range.indexOf(Regular(true))
        val c4IndexFalse = c4.variable.range.indexOf(Regular(false))
        val c4IndexTrue = c4.variable.range.indexOf(Regular(true))
        val startIndexFalse = startVar.range.indexOf(Regular(false))
        val accum1IndexFalse = accum1Var.range.indexOf(Regular(false))
        val accum1IndexTrue = accum1Var.range.indexOf(Regular(true))
        val accum2IndexFalse = accum2Var.range.indexOf(Regular(false))
        val accum2IndexTrue = accum2Var.range.indexOf(Regular(true))

        factor1.contents.size should equal (2)
        factor1.get(List(startIndexFalse, c1IndexFalse, accum1IndexFalse)) should equal (1.0)
        factor1.get(List(startIndexFalse, c1IndexTrue, accum1IndexTrue)) should equal (1.0)
        factor2.contents.size should equal (4)
        factor2.get(List(accum1IndexFalse, c2IndexFalse, accum2IndexFalse)) should equal (1.0)
        factor2.get(List(accum1IndexFalse, c2IndexTrue, accum2IndexTrue)) should equal (1.0)
        factor2.get(List(accum1IndexTrue, c2IndexFalse, accum2IndexTrue)) should equal (1.0)
        factor2.get(List(accum1IndexTrue, c2IndexTrue, accum2IndexTrue)) should equal (1.0)
        factor3.contents.size should equal (4)
        factor3.get(List(accum2IndexFalse, c3IndexFalse, c4IndexFalse)) should equal (1.0)
        factor3.get(List(accum2IndexFalse, c3IndexTrue, c4IndexTrue)) should equal (1.0)
        factor3.get(List(accum2IndexTrue, c3IndexFalse, c4IndexTrue)) should equal (1.0)
        factor3.get(List(accum2IndexTrue, c3IndexTrue, c4IndexTrue)) should equal (1.0)
      }
    }

    "given a fold with *" should {
      "produce the sequence of fold factors including *" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val e0f = Constant(false)
        val e0t = Constant(true)
        val e1 = Uniform(e0f, e0t)
        val e2 = Flip(0.2)
        val e3 = Flip(0.3)
        val e4 = FoldLeft(false, (b1: Boolean, b2: Boolean) => b1 || b2)(e1, e2, e3)
        pr.add(e0f)
        pr.add(e1)
        pr.add(e2)
        pr.add(e3)
        pr.add(e4)
        val c0f = cc(e0f)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c0f.generateRange()
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.generateRange()
        c4.makeNonConstraintFactors()

        val List(factor1, factor2, factor3) = c4.nonConstraintFactors
        factor1.variables.size should equal (3)
        val startVar = factor1.variables(0)
        factor1.variables(1) should equal (c1.variable)
        val accum1Var = factor1.variables(2)
        factor2.variables.size should equal (3)
        factor2.variables(0) should equal (accum1Var)
        factor2.variables(1) should equal (c2.variable)
        val accum2Var = factor2.variables(2)
        factor3.variables should equal (List(accum2Var, c3.variable, c4.variable))

        startVar.range should equal (List(Regular(false)))
        accum1Var.range.size should equal (2)
        accum1Var.valueSet.hasStar should equal (true)
        accum1Var.range.contains(Regular(false)) should equal (true)
        accum2Var.range.size should equal (3)
        accum2Var.valueSet.hasStar should equal (true)
        accum2Var.range.contains(Regular(false)) should equal (true)
        accum2Var.range.contains(Regular(true)) should equal (true)

        val c1IndexFalse = c1.variable.range.indexOf(Regular(false))
        val c1IndexStar = c1.variable.range.indexWhere(!_.isRegular)
        val c2IndexFalse = c2.variable.range.indexOf(Regular(false))
        val c2IndexTrue = c2.variable.range.indexOf(Regular(true))
        val c3IndexFalse = c3.variable.range.indexOf(Regular(false))
        val c3IndexTrue = c3.variable.range.indexOf(Regular(true))
        val c4IndexFalse = c4.variable.range.indexOf(Regular(false))
        val c4IndexTrue = c4.variable.range.indexOf(Regular(true))
        val c4IndexStar = c4.variable.range.indexWhere(!_.isRegular)
        val startIndexFalse = startVar.range.indexOf(Regular(false))
        val accum1IndexFalse = accum1Var.range.indexOf(Regular(false))
        val accum1IndexTrue = accum1Var.range.indexOf(Regular(true))
        val accum1IndexStar = accum1Var.range.indexWhere(!_.isRegular)
        val accum2IndexFalse = accum2Var.range.indexOf(Regular(false))
        val accum2IndexTrue = accum2Var.range.indexOf(Regular(true))
        val accum2IndexStar = accum2Var.range.indexWhere(!_.isRegular)

        factor1.contents.size should equal (2)
        factor1.get(List(startIndexFalse, c1IndexFalse, accum1IndexFalse)) should equal (1.0)
        factor1.get(List(startIndexFalse, c1IndexStar, accum1IndexStar)) should equal (1.0)
        factor2.contents.size should equal (4)
        factor2.get(List(accum1IndexFalse, c2IndexFalse, accum2IndexFalse)) should equal (1.0)
        factor2.get(List(accum1IndexFalse, c2IndexTrue, accum2IndexTrue)) should equal (1.0)
        factor2.get(List(accum1IndexStar, c2IndexFalse, accum2IndexStar)) should equal (1.0)
        factor2.get(List(accum1IndexStar, c2IndexTrue, accum2IndexStar)) should equal (1.0)
        factor3.contents.size should equal (6)
        factor3.get(List(accum2IndexFalse, c3IndexFalse, c4IndexFalse)) should equal (1.0)
        factor3.get(List(accum2IndexFalse, c3IndexTrue, c4IndexTrue)) should equal (1.0)
        factor3.get(List(accum2IndexTrue, c3IndexFalse, c4IndexTrue)) should equal (1.0)
        factor3.get(List(accum2IndexTrue, c3IndexTrue, c4IndexTrue)) should equal (1.0)
        factor3.get(List(accum2IndexStar, c3IndexFalse, c4IndexStar)) should equal (1.0)
        factor3.get(List(accum2IndexStar, c3IndexTrue, c4IndexStar)) should equal (1.0)
      }
    }

    "given an element whose expanded values are only *" should {
      "produce no factors" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val f = Flip(0.5)
        pr.add(f)
        val c1 = cc(f)
        c1.makeNonConstraintFactors()
        c1.nonConstraintFactors should be(empty)
      }
    }

    "given a non-trivial condition and constraint without *" should {
      "produce the correct constraint factors" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v1 = Select(0.2 -> 1, 0.3 -> 2, 0.5 -> 3)
        v1.setCondition((i: Int) => i != 2)
        v1.setConstraint(((i: Int) => i.toDouble))
        pr.add(v1)
        val c1 = cc(v1)
        c1.generateRange()

        val v1Vals = c1.variable.range
        val v11 = v1Vals indexOf Regular(1)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)

        c1.makeConstraintFactors(Lower)
        val List(condFactorL, constrFactorL) = c1.constraintLower
        condFactorL.get(List(v11)) should be(1.0 +- 0.000000001)
        condFactorL.get(List(v12)) should be(0.0 +- 0.000000001)
        condFactorL.get(List(v13)) should be(1.0 +- 0.000000001)
        constrFactorL.get(List(v11)) should be(1.0 +- 0.000000001)
        constrFactorL.get(List(v12)) should be(2.0 +- 0.000000001)
        constrFactorL.get(List(v13)) should be(3.0 +- 0.000000001)

        c1.makeConstraintFactors(Upper)
        val List(condFactorU, constrFactorU) = c1.constraintUpper
        condFactorU.get(List(v11)) should be(1.0 +- 0.000000001)
        condFactorU.get(List(v12)) should be(0.0 +- 0.000000001)
        condFactorU.get(List(v13)) should be(1.0 +- 0.000000001)
        constrFactorU.get(List(v11)) should be(1.0 +- 0.000000001)
        constrFactorU.get(List(v12)) should be(2.0 +- 0.000000001)
        constrFactorU.get(List(v13)) should be(3.0 +- 0.000000001)
      }
    }

    "given a non-trivial condition and constraint with *" should {
      "produce the correct lower and upper constraint factors" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val v01 = Constant(1)
        val v02 = Constant(2)
        val v03 = Constant(3)
        val v1 = Dist(0.2 -> v01, 0.3 -> v02, 0.5 -> v03)
        v1.setCondition((i: Int) => i != 2)
        v1.setConstraint(((i: Int) => i.toDouble))
        pr.add(v02)
        pr.add(v03)
        pr.add(v1)
        val c02 = cc(v02)
        val c03 = cc(v03)
        val c1 = cc(v1)
        c02.generateRange()
        c03.generateRange()
        c1.generateRange()

        val v1Vals = c1.variable.range
        val v1Star = v1Vals.indexWhere(!_.isRegular)
        val v12 = v1Vals indexOf Regular(2)
        val v13 = v1Vals indexOf Regular(3)

        c1.makeConstraintFactors(Lower)
        val List(condFactorL, constrFactorL) = c1.constraintLower
        condFactorL.get(List(v1Star)) should be(0.0 +- 0.000000001)
        condFactorL.get(List(v12)) should be(0.0 +- 0.000000001)
        condFactorL.get(List(v13)) should be(1.0 +- 0.000000001)
        constrFactorL.get(List(v1Star)) should be(0.0 +- 0.000000001)
        constrFactorL.get(List(v12)) should be(2.0 +- 0.000000001)
        constrFactorL.get(List(v13)) should be(3.0 +- 0.000000001)

        c1.makeConstraintFactors(Upper)
        val List(condFactorU, constrFactorU) = c1.constraintUpper
        condFactorU.get(List(v1Star)) should be(1.0 +- 0.000000001)
        condFactorU.get(List(v12)) should be(0.0 +- 0.000000001)
        condFactorU.get(List(v13)) should be(1.0 +- 0.000000001)
        constrFactorU.get(List(v1Star)) should be(1.0 +- 0.000000001)
        constrFactorU.get(List(v12)) should be(2.0 +- 0.000000001)
        constrFactorU.get(List(v13)) should be(3.0 +- 0.000000001)
      }
    }

    "given a constraint with a contingency without *" should {
      "create the appropriate extended factor for the contingent constraint" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e11 = Constant(true)("e1", ec1)
        val e12 = Constant(false)("e1", ec2)
        val e2 = Uniform(ec1, ec2)("e2", universe)
        universe.assertEvidence("e2.e1", Constraint((b: Boolean) => if (b) 0.6 else 0.3))
        pr.add(e11)
        pr.add(e12)
        pr.add(e2)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c11.makeConstraintFactors()
        c12.makeConstraintFactors()

        val e2Index1 = c2.variable.range.indexOf(Regular(ec1))
        val e2Index2 = c2.variable.range.indexOf(Regular(ec2))
        val List(factor1) = c11.constraintLower
        val List(factor2) = c12.constraintLower
        factor1.variables should equal (List(c2.variable, c11.variable))
        factor2.variables should equal (List(c2.variable, c12.variable))
        factor1.size should equal (2)
        factor1.get(List(e2Index1, 0)) should equal (0.6)
        factor1.get(List(e2Index2, 0)) should equal (1.0)
        factor2.size should equal (2)
        factor2.get(List(e2Index1, 0)) should equal (1.0)
        factor2.get(List(e2Index2, 0)) should equal (0.3 +- .0001)
      }
    }

    "given a constraint with a contingency with * in the contingency" should {
      "create the appropriate extended factor for the contingent constraint including *" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val pr = new Problem(cc)
        val ec1 = new EC1(universe)
        val ec2 = new EC1(universe)
        val e01 = Constant(ec1)
        val e02 = Constant(ec2)
        val e11 = Constant(true)("e1", ec1)
        val e12 = Constant(false)("e1", ec2)
        val e2 = Uniform(e01, e02)("e2", universe)
        universe.assertEvidence("e2.e1", Constraint((b: Boolean) => if (b) 0.6 else 0.3))
        pr.add(e02)
        pr.add(e11)
        pr.add(e12)
        pr.add(e2)
        val c02 = cc(e02)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        c02.generateRange()
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c11.makeConstraintFactors(Lower)
        c12.makeConstraintFactors(Lower)
        c11.makeConstraintFactors(Upper)
        c12.makeConstraintFactors(Upper)

        val e2IndexStar = c2.variable.range.indexWhere(!_.isRegular)
        val e2Index2 = c2.variable.range.indexOf(Regular(ec2))
        val List(factor1L) = c11.constraintLower
        val List(factor2L) = c12.constraintLower
        factor1L.variables should equal (List(c2.variable, c11.variable))
        factor2L.variables should equal (List(c2.variable, c12.variable))
        factor1L.size should equal (2)
        factor1L.get(List(e2IndexStar, 0)) should equal (0.6)
        factor1L.get(List(e2Index2, 0)) should equal (1.0)
        factor2L.size should equal (2)
        factor2L.get(List(e2IndexStar, 0)) should equal (0.3 +- .0001)
        factor2L.get(List(e2Index2, 0)) should equal (0.3 +- .0001)
        val List(factor1U) = c11.constraintUpper
        val List(factor2U) = c12.constraintUpper
        factor1U.variables should equal (List(c2.variable, c11.variable))
        factor2U.variables should equal (List(c2.variable, c12.variable))
        factor1U.size should equal (2)
        factor1U.get(List(e2IndexStar, 0)) should equal (1.0)
        factor1U.get(List(e2Index2, 0)) should equal (1.0)
        factor2U.size should equal (2)
        factor2U.get(List(e2IndexStar, 0)) should equal (1.0)
        factor2U.get(List(e2Index2, 0)) should equal (0.3 +- .0001)
      }
    }

  }

  class EC1(universe: Universe) extends ElementCollection
}
