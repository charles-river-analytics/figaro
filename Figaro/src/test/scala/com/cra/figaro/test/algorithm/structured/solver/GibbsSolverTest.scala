/*
 * GibbsSolverTest.scala
 * Test of a Gibbs sampling problem solver.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 21, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured.solver

import org.scalatest.{WordSpec, Matchers}
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.factored.gibbs.BlockSampler
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.solver._


class GibbsSolverTest extends WordSpec with Matchers {
  "Running a Gibbs solver" should {
    "correctly produce blocks" when {
      "given a problem that uses Apply" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Uniform(1, 2, 3)
        val e2 = Uniform(4, 5, 6)
        val e3 = Apply[Int, Int, Int](e1, e2, _ + _)
        val pr = new Problem(cc, List(e1, e2, e3))
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        // Make factors so variables have known parents
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        val v1 = c1.variable
        val v2 = c2.variable
        val v3 = c3.variable

        val solver = new GibbsSolver(pr, Set(), Set(v2 , v3), pr.components.flatMap(_.nonConstraintFactors), 1, 0, 1, BlockSampler.default)
        // Call initialize to set solver.variables so createBlocks may be called
        solver.initialize()
        solver.createBlocks().map(_.toSet) should contain theSameElementsAs(List(Set(v1, v3), Set(v2, v3)))
      }

      "given a problem that uses Chain" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Flip(0.3)
        val e2 = Uniform(1, 2, 3)
        val e3 = Uniform(4, 5, 6)
        val e4 = If(e1, e2, e3)
        val pr = new Problem(cc, List(e1, e2, e3, e4))
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val c4 = cc(e4)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c4.expand()
        c4.generateRange()
        // Make factors so variables have known parents
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c4.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        c4.makeConstraintFactors()
        val v1 = c1.variable
        val v2 = c2.variable
        val v3 = c3.variable
        val v4 = c4.variable

        val solver = new GibbsSolver(pr, Set(), Set(v4), pr.components.flatMap(_.nonConstraintFactors), 1, 0, 1, BlockSampler.default)
        // Call initialize to set solver.variables so createBlocks may be called
        solver.initialize()
        val v5 = (solver.variables -- Set(v1, v2, v3, v4)).head
        solver.createBlocks().map(_.toSet) should contain theSameElementsAs(List(Set(v1, v5), Set(v2, v4, v5), Set(v3, v4, v5)))
      }
    }
  }

  // Largely the same as VESolver tests, but modified to include tolerance and replacing Dist where needed
  "Running Gibbs without *" when {

    "given a flat model with no conditions or constraints" should {
      "produce the correct result over a single element" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        val pr = new Problem(cc, List(e2))
        pr.add(e1)
        pr.add(e3)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal (Set(c2))
        pr.solved should equal (true)
        val result = multiplyAll(pr.solution)
        result.variables should equal (List(c2.variable))
        result.size should equal (2)
        val c2IndexT = c2.variable.range.indexOf(Regular(true))
        val c2IndexF = c2.variable.range.indexOf(Regular(false))
        result.get(List(c2IndexT)) should be (0.6 +- tol)
        result.get(List(c2IndexF)) should be (0.4 +- tol)
      }

      "produce the correct result over multiple elements" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        val pr = new Problem(cc, List(e2, e3))
        pr.add(e1)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal (Set(c2, c3))
        val result = multiplyAll(pr.solution)
        result.variables.size should equal (2)
        val c2IndexT = c2.variable.range.indexOf(Regular(true))
        val c2IndexF = c2.variable.range.indexOf(Regular(false))
        val c3IndexT = c3.variable.range.indexOf(Regular(true))
        val c3IndexF = c3.variable.range.indexOf(Regular(false))
        result.size should equal (4)
        val var0 = result.variables(0)
        val var1 = result.variables(1)
        if (var0 == c2.variable) {
          var1 should equal (c3.variable)
          result.get(List(c2IndexT, c3IndexT)) should be (0.6 +- tol)
          result.get(List(c2IndexT, c3IndexF)) should be (0.0 +- tol)
          result.get(List(c2IndexF, c3IndexT)) should be (0.0 +- tol)
          result.get(List(c2IndexF, c3IndexF)) should be (0.4 +- tol)
        } else {
          var0 should equal (c3.variable)
          var1 should equal (c2.variable)
          result.get(List(c3IndexT, c2IndexT)) should be (0.6 +- tol)
          result.get(List(c3IndexT, c2IndexF)) should be (0.0 +- tol)
          result.get(List(c3IndexF, c2IndexT)) should be (0.0 +- tol)
          result.get(List(c3IndexF, c2IndexF)) should be (0.4 +- tol)
        }
      }
    }

    "given a condition on a dependent element" should {
      "produce the result with the correct probability" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.observe(true)
        val pr = new Problem(cc, List(e1))
        pr.add(e2)
        pr.add(e3)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal  (Set(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        val x3 = 0.25 * 0.3
        val x5 = 0.25 * 0.5
        val x7 = 0.25 * 0.7
        val x9 = 0.25 * 0.9
        val z = x3 + x5 + x7 + x9
        result.get(List(c1Index3)) should be ((x3 / z) +- tol)
        result.get(List(c1Index5)) should be ((x5 / z) +- tol)
        result.get(List(c1Index7)) should be ((x7 / z) +- tol)
        result.get(List(c1Index9)) should be ((x9 / z) +- tol)
      }
    }

    "given a constraint on a dependent element" should {
      "produce the result with the correct probability" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.addConstraint((b: Boolean) => if (b) 0.5 else 0.2)
        val pr = new Problem(cc, List(e1))
        pr.add(e2)
        pr.add(e3)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal  (Set(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        val x3 = 0.25 * (0.3 * 0.5 + 0.7 * 0.2)
        val x5 = 0.25 * (0.5 * 0.5 + 0.5 * 0.2)
        val x7 = 0.25 * (0.7 * 0.5 + 0.3 * 0.2)
        val x9 = 0.25 * (0.9 * 0.5 + 0.1 * 0.2)
        val z = x3 + x5 + x7 + x9
        result.get(List(c1Index3)) should be (x3 / z +- tol)
        result.get(List(c1Index5)) should be (x5 / z +- tol)
        result.get(List(c1Index7)) should be (x7 / z +- tol)
        result.get(List(c1Index9)) should be (x9 / z +- tol)
      }
    }

    "given two constraints on a dependent element" should {
      "produce the result with the correct probability" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e3.addConstraint((b: Boolean) => if (b) 0.5 else 0.2)
        e3.addConstraint((b: Boolean) => if (b) 0.4 else 0.1)
        val pr = new Problem(cc, List(e1))
        pr.add(e2)
        pr.add(e3)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal  (Set(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        val x3 = 0.25 * (0.3 * 0.5 * 0.4 + 0.7 * 0.2 * 0.1)
        val x5 = 0.25 * (0.5 * 0.5 * 0.4 + 0.5 * 0.2 * 0.1)
        val x7 = 0.25 * (0.7 * 0.5 * 0.4 + 0.3 * 0.2 * 0.1)
        val x9 = 0.25 * (0.9 * 0.5 * 0.4 + 0.1 * 0.2 * 0.1)
        val z = x3 + x5 + x7 + x9
        result.get(List(c1Index3)) should be (x3 / z +- tol)
        result.get(List(c1Index5)) should be (x5 / z +- tol)
        result.get(List(c1Index7)) should be (x7 / z +- tol)
        result.get(List(c1Index9)) should be (x9 / z +- tol)
      }
    }

    "given constraints on two dependent elements" should {
      "produce the result with the correct probability" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
        val e2 = Flip(e1)
        val e3 = Apply(e2, (b: Boolean) => b)
        e2.addConstraint((b: Boolean) => if (b) 0.5 else 0.2)
        e3.addConstraint((b: Boolean) => if (b) 0.4 else 0.1)
        val pr = new Problem(cc, List(e1))
        pr.add(e2)
        pr.add(e3)
        val c1 = cc(e1)
        val c2 = cc(e2)
        val c3 = cc(e3)
        c1.generateRange()
        c2.generateRange()
        c3.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c3.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal  (Set(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        val x3 = 0.25 * (0.3 * 0.5 * 0.4 + 0.7 * 0.2 * 0.1)
        val x5 = 0.25 * (0.5 * 0.5 * 0.4 + 0.5 * 0.2 * 0.1)
        val x7 = 0.25 * (0.7 * 0.5 * 0.4 + 0.3 * 0.2 * 0.1)
        val x9 = 0.25 * (0.9 * 0.5 * 0.4 + 0.1 * 0.2 * 0.1)
        val z = x3 + x5 + x7 + x9
        result.get(List(c1Index3)) should be (x3 / z +- tol)
        result.get(List(c1Index5)) should be (x5 / z +- tol)
        result.get(List(c1Index7)) should be (x7 / z +- tol)
        result.get(List(c1Index9)) should be (x9 / z +- tol)
      }
    }

    "given a contingent condition on an element" should {
      "produce the result with the correct probability" in {
        val universe = Universe.createNew()
        val cc = new ComponentCollection
        val ec1 = new EC1
        val ec2 = new EC1
        val e11 = Flip(0.6)("e1", ec1)
        val e12 = Flip(0.3)("e1", ec2)
        val e2 = Select(0.8 -> ec1, 0.2 -> ec2)("e2", universe)
        universe.assertEvidence("e2.e1", Observation(true))
        val pr = new Problem(cc, List(e2))
        pr.add(e11)
        pr.add(e12)
        val c11 = cc(e11)
        val c12 = cc(e12)
        val c2 = cc(e2)
        c11.generateRange()
        c12.generateRange()
        c2.generateRange()
        c11.makeNonConstraintFactors()
        c12.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c11.makeConstraintFactors()
        c12.makeConstraintFactors()
        c2.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

        pr.globals should equal (Set(c2))
        val result = multiplyAll(pr.solution)
        val c2Index1 = c2.variable.range.indexOf(Regular(ec1))
        val c2Index2 = c2.variable.range.indexOf(Regular(ec2))
        result.size should equal (2)
        val x1 = (0.8 * 0.6)
        val x2 = (0.2 * 0.3)
        val z = x1 + x2
        result.get(List(c2Index1)) should be ((x1 / z) +- tol)
        result.get(List(c2Index2)) should be ((x2 / z) +- tol)
      }
    }

    "with an element that uses another element multiple times, " +
      "always produce the same value for the different uses" in {
        Universe.createNew()
        val cc = new ComponentCollection
        val e1 = Flip(0.5)
        val e2 = Apply(e1, e1, (b1: Boolean, b2: Boolean) => b1 == b2)
        val pr = new Problem(cc, List(e2))
        pr.add(e1)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.generateRange()
        c1.makeNonConstraintFactors()
        c2.makeNonConstraintFactors()
        c1.makeConstraintFactors()
        c2.makeConstraintFactors()
        pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))
        val result = multiplyAll(pr.solution)
        val c2IndexT = c2.variable.range.indexOf(Regular(true))
        val c2IndexF = c2.variable.range.indexOf(Regular(false))
        result.get(List(c2IndexT)) should equal (1.0)
        result.get(List(c2IndexF)) should equal (0.0)
      }

    "with a constraint on an element that is used multiple times, only factor in the constraint once" in {
      Universe.createNew()
      val cc = new ComponentCollection
      val f1 = Flip(0.5)
      val f2 = Flip(0.3)
      val e1 = Apply(f1, f1, (b1: Boolean, b2: Boolean) => b1 == b2)
      val e2 = Apply(f1, f2, (b1: Boolean, b2: Boolean) => b1 == b2)
      val f3 = Flip(0.5)
      val d = If(f3, e1, e2)
      f1.setConstraint((b: Boolean) => if (b) 3.0; else 2.0)

      val pr = new Problem(cc, List(d))
      pr.add(f1)
      pr.add(f2)
      pr.add(e1)
      pr.add(e2)
      pr.add(f3)
      val cf1 = cc(f1)
      val cf2 = cc(f2)
      val ce1 = cc(e1)
      val ce2 = cc(e2)
      val cf3 = cc(f3)
      val cd = cc(d)
      cf1.generateRange()
      cf2.generateRange()
      ce1.generateRange()
      ce2.generateRange()
      cf3.generateRange()
      cd.expand()
      cd.generateRange()
      cf1.makeNonConstraintFactors()
      cf2.makeNonConstraintFactors()
      ce1.makeNonConstraintFactors()
      ce2.makeNonConstraintFactors()
      cf3.makeNonConstraintFactors()
      cd.makeNonConstraintFactors()
      cf1.makeConstraintFactors()
      cf2.makeConstraintFactors()
      ce1.makeConstraintFactors()
      ce2.makeConstraintFactors()
      cf3.makeConstraintFactors()
      cd.makeConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      // Probability that f1 is true = 0.6
      // Probability that e1 is true = 1.0
      // Probability that e2 is true = 0.6 * 0.3 + 0.4 * 0.7 = 0.46
      // Probability that d is true = 0.5 * 1 + 0.5 * 0.46 = 0.73
      val result = multiplyAll(pr.solution)
      val dIndexT = cd.variable.range.indexOf(Regular(true))
      val dIndexF = cd.variable.range.indexOf(Regular(false))
      val pT = result.get(List(dIndexT))
      val pF = result.get(List(dIndexF))
      (pT / (pT + pF)) should be (0.73 +- tol)
    }

    "with elements that are not used by the query or evidence, produce the correct result" in {
      val u1 = Universe.createNew()
      val cc = new ComponentCollection
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      val pr = new Problem(cc, List(f))
      pr.add(u)
      pr.add(a)
      val cu = cc(u)
      val cf = cc(f)
      val ca = cc(a)
      cu.generateRange()
      cf.generateRange()
      ca.expand()
      ca.generateRange()
      cu.makeNonConstraintFactors()
      cf.makeNonConstraintFactors()
      ca.makeNonConstraintFactors()
      cu.makeConstraintFactors()
      cf.makeConstraintFactors()
      ca.makeConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))
      val result = multiplyAll(pr.solution)
      val fIndexT = cf.variable.range.indexOf(Regular(true))
      val fIndexF = cf.variable.range.indexOf(Regular(false))
      val pT = result.get(List(fIndexT))
      val pF = result.get(List(fIndexF))
      (pT / (pT + pF)) should be (0.6 +- tol)
    }

    "with a model using chain and no conditions or constraints, when the outcomes are at the top level, produce the correct answer" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e4))
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      c4.expand()
      c4.generateRange()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.makeNonConstraintFactors()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))
      val result = multiplyAll(pr.solution)
      val c4Index1 = c4.variable.range.indexOf(Regular(1))
      result.get(List(c4Index1)) should be ((0.3 * 0.1 + 0.7 * 0.7) +- tol)
    }

    "with a model using chain and no conditions or constraints, when the outcomes are nested, produce the correct answer" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e4))
      pr.add(e1)
      val c1 = cc(e1)
      val c4 = cc(e4)
      c1.generateRange()
      c4.expand()
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.subproblems.values.foreach(_.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default))))
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))
      val result = multiplyAll(pr.solution)
      val c4Index1 = c4.variable.range.indexOf(Regular(1))
      result.get(List(c4Index1)) should be ((0.3 * 0.1 + 0.7 * 0.7) +- tol)
    }

    "with a model using chain and a condition on the result, when the outcomes are at the top level, correctly condition the parent" in {
      Universe.createNew()
      val e1= Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      e4.observe(1)

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
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
      c4.expand()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      val result = multiplyAll(pr.solution)
      val c1IndexT = c1.variable.range.indexOf(Regular(true))
      val c1IndexF = c1.variable.range.indexOf(Regular(false))
      val pT = result.get(List(c1IndexT))
      val pF = result.get(List(c1IndexF))
      (pT / (pT + pF)) should be ((0.3 * 0.1 / (0.3 * 0.1 + 0.7 * 0.7)) +- tol)
    }

    "with a model using chain and a condition on the result, when the outcomes are nested, correctly condition the parent" in {
      Universe.createNew()
      val e1= Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      e4.observe(1)

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      pr.add(e4)
      val c1 = cc(e1)
      val c4 = cc(e4)
      c1.generateRange()
      c4.expand()
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.subproblems.values.foreach(_.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default))))
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      val result = multiplyAll(pr.solution)
      val c1IndexT = c1.variable.range.indexOf(Regular(true))
      val c1IndexF = c1.variable.range.indexOf(Regular(false))
      val pT = result.get(List(c1IndexT))
      val pF = result.get(List(c1IndexF))
      (pT / (pT + pF)) should be ((0.3 * 0.1 / (0.3 * 0.1 + 0.7 * 0.7)) +- tol)
    }

    "with a model using chain and a condition on one of the outcome elements, when the outcomes are at the top level, correctly condition the result" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      e2.observe(1)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e4))
      pr.add(e1)
      pr.add(e2)
      pr.add(e3)
      val c1 = cc(e1)
      val c2 = cc(e2)
      val c3 = cc(e3)
      val c4 = cc(e4)
      c1.generateRange()
      c2.generateRange()
      c3.generateRange()
      c4.expand()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      val result = multiplyAll(pr.solution)
      val c4Index1 = c4.variable.range.indexOf(Regular(1))
      val c4Index2 = c4.variable.range.indexOf(Regular(2))
      val c4Index3 = c4.variable.range.indexOf(Regular(3))
      val p1 = result.get(List(c4Index1))
      val p2 = result.get(List(c4Index2))
      val p3 = result.get(List(c4Index3))
      (p1 / (p1 + p2 + p3)) should be ((0.3 * 1 + 0.7 * 0.7) +- tol)
    }

    "with a model using chain and a condition on one of the outcome elements, when the outcomes are at the top level, " +
    "not change the belief about the parent" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      e2.observe(1)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
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
      c4.expand()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      val result = multiplyAll(pr.solution)
      val c1IndexT = c1.variable.range.indexOf(Regular(true))
      val c1IndexF = c1.variable.range.indexOf(Regular(false))
      val pT = result.get(List(c1IndexT))
      val pF = result.get(List(c1IndexF))
      (pT / (pT + pF)) should be (0.3 +- tol)
    }

    "with a model using chain and a condition on one of the outcome elements, when the outcomes are nested, correctly condition the result" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      e2.observe(1)

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e4))
      pr.add(e1)
      val c1 = cc(e1)
      val c4 = cc(e4)
      c1.generateRange()
      c4.expand()
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.subproblems.values.foreach(_.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default))))
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      val result = multiplyAll(pr.solution)
      val c4Index1 = c4.variable.range.indexOf(Regular(1))
      val c4Index2 = c4.variable.range.indexOf(Regular(2))
      val c4Index3 = c4.variable.range.indexOf(Regular(3))
      val p1 = result.get(List(c4Index1))
      val p2 = result.get(List(c4Index2))
      val p3 = result.get(List(c4Index3))
      (p1 / (p1 + p2 + p3)) should be ((0.3 * 1 + 0.7 * 0.7) +- tol)
    }

    "with a model using chain and a condition on one of the outcome elements, when the outcomes are nested, " +
    "not change the belief about the parent" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = Select(0.1 -> 1, 0.9 -> 2)
      val e3 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val e4 = Chain(e1, (b: Boolean) => if (b) e2; else e3)
      e2.observe(1)

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      pr.add(e4)
      val c1 = cc(e1)
      val c4 = cc(e4)
      c1.generateRange()
      c4.expand()
      val c2 = cc(e2)
      val c3 = cc(e3)
      c2.generateRange()
      c3.generateRange()
      c4.generateRange()
      c1.makeConstraintFactors()
      c2.makeConstraintFactors()
      c3.makeConstraintFactors()
      c4.makeConstraintFactors()
      c1.makeNonConstraintFactors()
      c2.makeNonConstraintFactors()
      c3.makeNonConstraintFactors()
      c4.subproblems.values.foreach(_.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default))))
      c4.makeNonConstraintFactors()
      pr.solve(new ConstantStrategy(gibbs(10000, 0, 1, BlockSampler.default)))

      val result = multiplyAll(pr.solution)
      val c1IndexT = c1.variable.range.indexOf(Regular(true))
      val c1IndexF = c1.variable.range.indexOf(Regular(false))
      val pT = result.get(List(c1IndexT))
      val pF = result.get(List(c1IndexF))
      (pT / (pT + pF)) should be (0.3 +- tol)
    }

  }

  def multiplyAll(factors: List[Factor[Double]]): Factor[Double] = factors.foldLeft(Factory.unit(SumProductSemiring()))(_.product(_))

  class EC1 extends ElementCollection { }

  val tol = 0.025
}
