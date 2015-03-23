package com.cra.figaro.test.experimental.structured.solver

import org.scalatest.{WordSpec, Matchers}
import com.cra.figaro.language._
import com.cra.figaro.experimental.structured._
import solver._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.algorithm.factored.factors.Factor
import com.cra.figaro.experimental.structured.factory.Factory
import com.cra.figaro.algorithm.factored.factors.SumProductSemiring
import com.cra.figaro.algorithm.lazyfactored.Regular

class VESolverTest extends WordSpec with Matchers {
  "Running VariableElimination without *" when {
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
        pr.solve(variableElimination)

        pr.globals should equal (List(c2))
        val result = multiplyAll(pr.solution)
        result.variables should equal (List(c2.variable))
        result.size should equal (2)
        val c2IndexT = c2.variable.range.indexOf(Regular(true))
        val c2IndexF = c2.variable.range.indexOf(Regular(false))
        result.get(List(c2IndexT)) should be (0.6 +- 0.00000001)
        result.get(List(c2IndexF)) should be (0.4 +- 0.00000001)
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
        pr.solve(variableElimination)

        pr.globals.toSet should equal (Set(c2, c3))
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
          result.get(List(c2IndexT, c3IndexT)) should equal (0.6)
          result.get(List(c2IndexT, c3IndexF)) should equal (0.0)
          result.get(List(c2IndexF, c3IndexT)) should equal (0.0)
          result.get(List(c2IndexF, c3IndexF)) should equal (0.4)
        } else {
          var0 should equal (c3.variable)
          var1 should equal (c2.variable)
          result.get(List(c3IndexT, c2IndexT)) should equal (0.6)
          result.get(List(c3IndexT, c2IndexF)) should equal (0.0)
          result.get(List(c3IndexF, c2IndexT)) should equal (0.0)
          result.get(List(c3IndexF, c2IndexF)) should equal (0.4)
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
        c3.makeConstraintFactors()
        pr.solve(variableElimination)

        pr.globals should equal  (List(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        result.get(List(c1Index3)) should be ((0.25 * 0.3) +- 0.000000001)
        result.get(List(c1Index5)) should be ((0.25 * 0.5) +- 0.000000001)
        result.get(List(c1Index7)) should be ((0.25 * 0.7) +- 0.000000001)
        result.get(List(c1Index9)) should be ((0.25 * 0.9) +- 0.000000001)
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
        c3.makeConstraintFactors()
        pr.solve(variableElimination)

        pr.globals should equal  (List(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        result.get(List(c1Index3)) should be ((0.25 * (0.3 * 0.5 + 0.7 * 0.2)) +- 0.000000001)
        result.get(List(c1Index5)) should be ((0.25 * (0.5 * 0.5 + 0.5 * 0.2)) +- 0.000000001)
        result.get(List(c1Index7)) should be ((0.25 * (0.7 * 0.5 + 0.3 * 0.2)) +- 0.000000001)
        result.get(List(c1Index9)) should be ((0.25 * (0.9 * 0.5 + 0.1 * 0.2)) +- 0.000000001)
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
        c3.makeConstraintFactors()
        pr.solve(variableElimination)

        pr.globals should equal  (List(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        result.get(List(c1Index3)) should be ((0.25 * (0.3 * 0.5 * 0.4 + 0.7 * 0.2 * 0.1)) +- 0.000000001)
        result.get(List(c1Index5)) should be ((0.25 * (0.5 * 0.5 * 0.4 + 0.5 * 0.2 * 0.1)) +- 0.000000001)
        result.get(List(c1Index7)) should be ((0.25 * (0.7 * 0.5 * 0.4 + 0.3 * 0.2 * 0.1)) +- 0.000000001)
        result.get(List(c1Index9)) should be ((0.25 * (0.9 * 0.5 * 0.4 + 0.1 * 0.2 * 0.1)) +- 0.000000001)
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
        c2.makeConstraintFactors()
        c3.makeConstraintFactors()
        pr.solve(variableElimination)

        pr.globals should equal  (List(c1))
        val result = multiplyAll(pr.solution)
        val c1Index3 = c1.variable.range.indexOf(Regular(0.3))
        val c1Index5 = c1.variable.range.indexOf(Regular(0.5))
        val c1Index7 = c1.variable.range.indexOf(Regular(0.7))
        val c1Index9 = c1.variable.range.indexOf(Regular(0.9))
        result.size should equal (4)
        result.get(List(c1Index3)) should be ((0.25 * (0.3 * 0.5 * 0.4 + 0.7 * 0.2 * 0.1)) +- 0.000000001)
        result.get(List(c1Index5)) should be ((0.25 * (0.5 * 0.5 * 0.4 + 0.5 * 0.2 * 0.1)) +- 0.000000001)
        result.get(List(c1Index7)) should be ((0.25 * (0.7 * 0.5 * 0.4 + 0.3 * 0.2 * 0.1)) +- 0.000000001)
        result.get(List(c1Index9)) should be ((0.25 * (0.9 * 0.5 * 0.4 + 0.1 * 0.2 * 0.1)) +- 0.000000001)
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
        pr.solve(variableElimination)

        pr.globals should equal (List(c2))
        val result = multiplyAll(pr.solution)
        val c2Index1 = c2.variable.range.indexOf(Regular(ec1))
        val c2Index2 = c2.variable.range.indexOf(Regular(ec2))
        result.size should equal (2)
        result.get(List(c2Index1)) should be ((0.8 * 0.6) +- 0.000000001)
        result.get(List(c2Index2)) should be ((0.2 * 0.3) +- 0.000000001)
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
        pr.solve(variableElimination)
println("Solution:")
pr.solution.foreach((f: Factor[Double]) => println(f.toReadableString))
        val result = multiplyAll(pr.solution)
        val c2IndexT = c2.variable.range.indexOf(Regular(true))
        val c2IndexF = c2.variable.range.indexOf(Regular(false))
        result.get(List(c2IndexT)) should be (0.5 +- 0.000000001)
        result.get(List(c2IndexF)) should be (0.0 +- 0.000000001)
      }
/*
    "with a constraint on an element that is used multiple times, only factor in the constraint once" in {
      Universe.createNew()
      val f1 = Flip(0.5)
      val f2 = Flip(0.3)
      val e1 = f1 === f1
      val e2 = f1 === f2
      val d = Dist(0.5 -> e1, 0.5 -> e2)
      f1.setConstraint((b: Boolean) => if (b) 3.0; else 2.0)
      // Probability that f1 is true = 0.6
      // Probability that e1 is true = 1.0
      // Probability that e2 is true = 0.6 * 0.3 + 0.4 * 0.7 = 0.46
      // Probability that d is true = 0.5 * 1 + 0.5 * 0.46 = 0.73
      test(d, (b: Boolean) => b, 0.73)
    }

    "with elements that are not used by the query or evidence, produce the correct result" in {
      val u1 = Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      val a = If(f, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
      test(f, (b: Boolean) => b, 0.6)
    }

    "on a different universe from the current universe, produce the correct result" in {
      val u1 = Universe.createNew()
      val u = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
      val f = Flip(u)
      Universe.createNew()
      val tolerance = 0.0000001
      val algorithm = VariableElimination(f)(u1)
      algorithm.start()
      algorithm.probability(f, (b: Boolean) => b) should be(0.6 +- tolerance)
      algorithm.kill()
    }

    "with a model using chain and no conditions or constraints, produce the correct answer" in {
      Universe.createNew()
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      test(c, (i: Int) => i == 1, 0.3 * 0.1 + 0.7 * 0.7)
    }

    "with a model using chain and a condition on the result, correctly condition the parent" in {
      Universe.createNew()
      val f = Flip(0.3)
      val s1 = Select(0.1 -> 1, 0.9 -> 2)
      val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
      val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
      c.observe(1)
      test(f, (b: Boolean) => b, 0.3 * 0.1 / (0.3 * 0.1 + 0.7 * 0.7))
    }

    "with a model using chain and a condition on one of the outcome elements, correctly condition the result " +
      "but not change the belief about the parent" in {
        Universe.createNew()
        val f = Flip(0.3)
        val s1 = Select(0.1 -> 1, 0.9 -> 2)
        val s2 = Select(0.7 -> 1, 0.2 -> 2, 0.1 -> 3)
        val c = Chain(f, (b: Boolean) => if (b) s1; else s2)
        s1.observe(1)
        test(c, (i: Int) => i == 1, 0.3 * 1 + 0.7 * 0.7)
        test(f, (b: Boolean) => b, 0.3)
      }

    "with a dependent universe, correctly take into account probability of evidence in the dependent universe" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Flip(0.2)
      val dependentUniverse = new Universe(List(x, y))
      val u1 = Uniform(0.0, 1.0)("", dependentUniverse)
      val u2 = Uniform(0.0, 2.0)("", dependentUniverse)
      val a = CachingChain(x, y, (x: Boolean, y: Boolean) => if (x || y) u1; else u2)("a", dependentUniverse)
      val condition = (d: Double) => d < 0.5
      val ve = VariableElimination(List((dependentUniverse, List(NamedEvidence("a", Condition(condition))))), x)
      ve.start()
      val peGivenXTrue = 0.5
      val peGivenXFalse = 0.2 * 0.5 + 0.8 * 0.25
      val unnormalizedPXTrue = 0.1 * peGivenXTrue
      val unnormalizedPXFalse = 0.9 * peGivenXFalse
      val pXTrue = unnormalizedPXTrue / (unnormalizedPXTrue + unnormalizedPXFalse)
      ve.probability(x, true) should be(pXTrue +- 0.01)
      ve.kill()
    }

    "with a contingent condition, correctly take into account the contingency" in {
      Universe.createNew()
      val x = Flip(0.1)
      val y = Flip(0.2)
      y.setCondition((b: Boolean) => b, List(Element.ElemVal(x, true)))
      // Probability of y should be (0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8) (because the case where x is true and y is false has been ruled out)
      val ve = VariableElimination(y)
      ve.start()
      ve.probability(y, true) should be(((0.1 * 0.2 + 0.9 * 0.2) / (0.1 * 0.2 + 0.9 * 0.2 + 0.9 * 0.8)) +- 0.0000000001)
    }

*/
  }

  def multiplyAll(factors: List[Factor[Double]]): Factor[Double] = factors.foldLeft(Factory.unit(SumProductSemiring))(_.product(_, SumProductSemiring))

  class EC1 extends ElementCollection { }
}
