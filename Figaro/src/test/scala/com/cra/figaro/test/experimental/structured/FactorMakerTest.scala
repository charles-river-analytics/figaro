package com.cra.figaro.test.experimental.structured

import org.scalatest.{WordSpec, Matchers}
import com.cra.figaro.language._
import com.cra.figaro.experimental.structured._
import com.cra.figaro.algorithm.lazyfactored.{Star, Regular}
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.atomic.continuous.Dirichlet

class FactorMakerTest extends WordSpec with Matchers {
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

    "given a simple flip" should {
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

    "given a parameterized flip" should {
      "produce the factor where the probability of true is equal to the MAP value of the parameter" in {
        Universe.createNew()
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
        c2.makeNonConstraintFactors()

        val List(factor) = c2.nonConstraintFactors
        factor.variables should equal (List(c2.variable))
        factor.size should equal (2)
        val trueIndex = c2.variable.range.indexOf(Regular(true))
        factor.get(List(trueIndex)) should equal (v1.MAPValue)
        factor.get(List(1 - trueIndex)) should equal (1 - v1.MAPValue)
      }
    }

    "given a simple select" should {
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

    "given a parameterized select" should {
      "produce the factor where the probability of the outcomes is equal to the MAP value of the parameter" in {
        Universe.createNew()
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
        c2.makeNonConstraintFactors()

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

/*
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
          val selectFactor :: outcomeFactors = Factory.make(v3)
          outcomeFactors.size should equal(2)
          val v1Factor = outcomeFactors(v1Index)
          val v2Factor = outcomeFactors(v2Index)
          selectFactor.get(List(v1Index)) should equal(0.3)
          selectFactor.get(List(v2Index)) should equal(0.7)
          v1Factor.get(List(v1Index, v1TrueIndex, v3TrueIndex)) should equal(1.0)
          v1Factor.get(List(v1Index, v1FalseIndex, v3TrueIndex)) should equal(0.0)
          v1Factor.get(List(v1Index, v1TrueIndex, v3FalseIndex)) should equal(0.0)
          v1Factor.get(List(v1Index, v1FalseIndex, v3FalseIndex)) should equal(1.0)
          for { i <- 0 to 1; j <- 0 to 1 } v1Factor.get(List(v2Index, i, j)) should equal(1.0)
          v2Factor.get(List(v2Index, 0, v3FalseIndex)) should equal(1.0)
          v2Factor.get(List(v2Index, 0, v3TrueIndex)) should equal(0.0)
          for { i <- 0 to 1} v2Factor.get(List(v1Index, 0, i)) should equal(1.0)
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
          selectFactor.get(List(v102, v204, 0)) should be(0.2 / 0.6 +- 0.0001)
          selectFactor.get(List(v102, v204, 1)) should be(0.4 / 0.6 +- 0.0001)
          selectFactor.get(List(v102, v206, 0)) should be(0.2 / 0.8 +- 0.0001)
          selectFactor.get(List(v102, v206, 1)) should be(0.6 / 0.8 +- 0.0001)
          selectFactor.get(List(v108, v204, 0)) should be(0.8 / 1.2 +- 0.0001)
          selectFactor.get(List(v108, v204, 1)) should be(0.4 / 1.2 +- 0.0001)
          selectFactor.get(List(v108, v206, 0)) should be(0.8 / 1.4 +- 0.0001)
          selectFactor.get(List(v108, v206, 1)) should be(0.6 / 1.4 +- 0.0001)
          v1Factor.get(List(0, v3t, v5t)) should equal(1.0)
          v1Factor.get(List(0, v3f, v5t)) should equal(0.0)
          v1Factor.get(List(0, v3t, v5f)) should equal(0.0)
          v1Factor.get(List(0, v3f, v5f)) should equal(1.0)
          for { i <- 0 to 1; j <- 0 to 1 } v1Factor.get(List(1, i, j)) should equal(1.0)
          v2Factor.get(List(1, 0, v5f)) should equal(1.0)
          v2Factor.get(List(1, 0, v5t)) should equal(0.0)
          for { i <- 0 to 0; j <- 0 to 1 } v2Factor.get(List(0, i, j)) should equal(1.0)
        }
    }

    "given an atomic not in the factor" should {
      "automatically sample the element" in {
        Universe.createNew()
        val v1 = Normal(0.0, 1.0)
        Values()(v1)
        val factor = Factory.make(v1)
        factor(0).size should equal(ParticleGenerator.defaultArgSamples)
        factor(0).get(List(0)) should equal(1.0/ParticleGenerator.defaultArgSamples)
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

        v4Factor.get(List(v1t, v21, 0, v41)) should equal(1.0)
        v4Factor.get(List(v1t, v22, 0, v41)) should equal(0.0)
        v4Factor.get(List(v1t, v21, 0, v42)) should equal(0.0)
        v4Factor.get(List(v1t, v22, 0, v42)) should equal(1.0)
        v4Factor.get(List(v1t, v21, 0, v43)) should equal(0.0)
        v4Factor.get(List(v1t, v22, 0, v43)) should equal(0.0)
        v4Factor.get(List(v1f, v21, 0, v41)) should equal(0.0)
        v4Factor.get(List(v1f, v22, 0, v41)) should equal(0.0)
        v4Factor.get(List(v1f, v21, 0, v42)) should equal(0.0)
        v4Factor.get(List(v1f, v22, 0, v42)) should equal(0.0)
        v4Factor.get(List(v1f, v21, 0, v43)) should equal(1.0)
        v4Factor.get(List(v1f, v22, 0, v43)) should equal(1.0)

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

        v2Factor.get(List(v1t, v3t, v4t, v2t)) should equal(1.0)
        v2Factor.get(List(v1t, v3t, v4f, v2t)) should equal(1.0)
        v2Factor.get(List(v1t, v3f, v4t, v2t)) should equal(0.0)
        v2Factor.get(List(v1t, v3f, v4f, v2t)) should equal(0.0)
        v2Factor.get(List(v1t, v3t, v4t, v2f)) should equal(0.0)
        v2Factor.get(List(v1t, v3t, v4f, v2f)) should equal(0.0)
        v2Factor.get(List(v1t, v3f, v4t, v2f)) should equal(1.0)
        v2Factor.get(List(v1t, v3f, v4f, v2f)) should equal(1.0)
        v2Factor.get(List(v1f, v3t, v4t, v2t)) should equal(1.0)
        v2Factor.get(List(v1f, v3t, v4f, v2t)) should equal(0.0)
        v2Factor.get(List(v1f, v3f, v4t, v2t)) should equal(1.0)
        v2Factor.get(List(v1f, v3f, v4f, v2t)) should equal(0.0)
        v2Factor.get(List(v1f, v3t, v4t, v2f)) should equal(0.0)
        v2Factor.get(List(v1f, v3t, v4f, v2f)) should equal(1.0)
        v2Factor.get(List(v1f, v3f, v4t, v2f)) should equal(0.0)
        v2Factor.get(List(v1f, v3f, v4f, v2f)) should equal(1.0)
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
        val List(factor) = Factory.make(v3)
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
        val List(factor) = Factory.make(v4)
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
        val List(factor) = Factory.make(v5)
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
        val List(factor) = Factory.make(v6)

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
        val List(condFactor, constrFactor, _) = Factory.make(v1)
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
        val factors = Factory.make(f)
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
        val List(factor) = Factory.make(v2)
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

        val factor = Factory.make(v4)
        val List(v4Factor) = Factory.combineFactors(factor, SumProductSemiring, true)

        v4Factor.get(List(v10, 0, v30, v40)) should equal(0.0)
        v4Factor.get(List(v10, 0, v31, v40)) should equal(0.0)
        v4Factor.get(List(v10, 0, v30, v41)) should equal(0.0)
        v4Factor.get(List(v10, 0, v31, v41)) should equal(0.0)
        v4Factor.get(List(v10, 0, v30, v42)) should equal(1.0)
        v4Factor.get(List(v10, 0, v31, v42)) should equal(1.0)
        v4Factor.get(List(v11, 0, v30, v40)) should equal(1.0)
        v4Factor.get(List(v11, 0, v31, v40)) should equal(0.0)
        v4Factor.get(List(v11, 0, v30, v41)) should equal(0.0)
        v4Factor.get(List(v11, 0, v31, v41)) should equal(1.0)
        v4Factor.get(List(v11, 0, v30, v42)) should equal(0.0)
        v4Factor.get(List(v11, 0, v31, v42)) should equal(0.0)
        v4Factor.get(List(v12, 0, v30, v40)) should equal(0.0)
        v4Factor.get(List(v12, 0, v31, v40)) should equal(0.0)
        v4Factor.get(List(v12, 0, v30, v41)) should equal(0.0)
        v4Factor.get(List(v12, 0, v31, v41)) should equal(0.0)
        v4Factor.get(List(v12, 0, v30, v42)) should equal(1.0)
        v4Factor.get(List(v12, 0, v31, v42)) should equal(1.0)
      }
    }

*/
  }
}
