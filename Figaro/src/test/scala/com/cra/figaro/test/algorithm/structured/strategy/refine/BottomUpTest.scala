/*
 * BottomUpTest.scala
 * Tests for bottom-up strategies.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Oct 18, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured.strategy.refine

import com.cra.figaro.algorithm.factored.factors.BasicFactor
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.structured.strategy.refine._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous
import com.cra.figaro.library.atomic.continuous.{Beta, Normal}
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.compound.If
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable

class BottomUpTest extends WordSpec with Matchers {
  "A complete bottom-up strategy" should {
    "create ranges for all components" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = If(e1, Constant(0), discrete.Uniform(2, 3, 4))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e2))
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.range.regularValues should equal(Set(true, false))
      c1.range.hasStar should be(false)
      c2.range.regularValues should equal(Set(0, 2, 3, 4))
      c2.range.hasStar should be(false)
      val c1t = cc(c2.subproblems(true).target)
      val c1f = cc(c2.subproblems(false).target)
      c1t.range.regularValues should equal(Set(0))
      c1t.range.hasStar should be(false)
      c1f.range.regularValues should equal(Set(2, 3, 4))
      c1f.range.hasStar should be(false)
    }

    "sample continuous components with the rangeSizer" in {
      Universe.createNew()
      val e1 = Normal(0, 1)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      val rangeSizer = (pc: ProblemComponent[_]) => 30
      new BottomUpStrategy(pr, rangeSizer, false, pr.targetComponents).execute()

      cc(e1).variable.size should be(30)
    }

    "create non-constraint factors for all components" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = If(e1, Constant(0), discrete.Uniform(2, 3, 4))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e2))
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

      val c1 = cc(e1)
      val c2 = cc(e2)
      // Each component has 1 factor
      // Chain components additionally have 1 factor per subproblem
      c1.nonConstraintFactors should have size 1
      c2.nonConstraintFactors should have size 3

      val c1t = cc(c2.subproblems(true).target)
      val c1f = cc(c2.subproblems(false).target)
      c1t.nonConstraintFactors should have size 1
      c1f.nonConstraintFactors should have size 1
    }

    "create upper and lower bound constraint factors for top-level components" in {
      Universe.createNew()
      val e1 = discrete.Uniform(1, 2, 3)
      e1.addConstraint((i: Int) => 1.0 / i)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

      val c1 = cc(e1)
      c1.constraintFactors(Lower) should have size 1
      c1.constraintFactors(Upper) should have size 1
    }

    // TODO parameters seemingly don't work in SFI
    /*"conditionally make parameterized factors" when {
      "parameterized is set to true" in {
        Universe.createNew()
        val e1 = Beta(1, 2)
        val e2 = Flip(e1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1, e2))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()
      }

      "parameterized is set to false" in {
        Universe.createNew()
        val e1 = Beta(1, 2)
        val e2 = Flip(e1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1, e2))
        new BottomUpStrategy(pr, defaultRangeSizer, true, pr.targetComponents).execute()
      }
    }*/

    "preserve solutions to solved subproblems" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = If(e1, Select(0.1 -> 1, 0.9 -> 2), discrete.Uniform(3, 4))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1, e2))
      val c1 = cc(e1)
      val c2 = cc(e2)
      c1.generateRange()
      c2.expand()
      // Decompose and solve the subproblem corresponding to true
      val spr = c2.subproblems(true)
      new BottomUpStrategy(spr, defaultRangeSizer, false, spr.targetComponents).execute()
      new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
      // This should not get rid of the solution
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

      spr.solved should be(true)
      val solution = spr.solution.head
      val range = cc(spr.target).variable.range
      solution.get(List(range.indexOf(Regular(1)))) should be(0.1 +- 0.000000001)
      solution.get(List(range.indexOf(Regular(2)))) should be(0.9 +- 0.000000001)
    }

    "recursively mark problems as unsolved" when {
      // A "solution" which we will artificially place in problems to make sure it gets removed
      val dummySolution = List(new BasicFactor[Double](List(), List()))

      "executed on a top-level problem with no subproblems" in {
        Universe.createNew()
        val e1 = Flip(0.3)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        pr.solved = true
        pr.solution = dummySolution
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        pr.solved should be(false)
        pr.solution should be(empty)
      }

      "executed recursively on a nested problem" in {
        Universe.createNew()
        val e1 = Flip(0.3)
        val e2 = If(e1, Select(0.1 -> 1, 0.9 -> 2), discrete.Uniform(3, 4))
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1, e2))
        // Create subproblems without refining them
        new PartialBottomUpStrategy(0, pr, defaultRangeSizer, false, pr.targetComponents).execute()

        pr.solved = true
        pr.solution = dummySolution
        for((_, subproblem) <- cc(e2).subproblems) {
          subproblem.solved = true
          subproblem.solution = dummySolution
        }
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        pr.solved should be(false)
        pr.solution should be(empty)
        for((_, subproblem) <- cc(e2).subproblems) {
          subproblem.solved should be(false)
          subproblem.solution should be(empty)
        }
      }

      "executed directly on a nested problem already marked as unsolved" in {
        Universe.createNew()
        val e1 = Flip(0.3)
        val e2 = If(e1, Select(0.1 -> 1, 0.9 -> 2), discrete.Uniform(3, 4))
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1, e2))
        // Create subproblems without refining them
        new PartialBottomUpStrategy(0, pr, defaultRangeSizer, false, pr.targetComponents).execute()

        // We don't mark the nested problems as solved because this test is to ensure that we recursively go up the
        // problem tree without stopping at unsolved components
        pr.solved = true
        pr.solution = dummySolution
        new BottomUpStrategy(cc(e2).subproblems(true), defaultRangeSizer, false, pr.targetComponents).execute()

        pr.solved should be(false)
        pr.solution should be(empty)
      }
    }

    "mark visited components as done" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = If(e1, Select(0.1 -> 1, 0.9 -> 2), discrete.Uniform(3, 4))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1, e2))
      val done = mutable.Set[ProblemComponent[_]]()
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents, done).execute()
      done.size should be(4)
    }

    "not decompose components in the done set" in {
      Universe.createNew()
      val e1 = discrete.Uniform(1, 2, 3)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      val c1 = cc(e1)
      val done = mutable.Set[ProblemComponent[_]](c1)
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents, done).execute()

      c1.variable.valueSet.regularValues should be(empty)
      c1.nonConstraintFactors should be(empty)
      c1.constraintFactors(Lower) should be(empty)
    }

    "correctly mark components as fully enumerated and refined" when {
      "a top-level component has finite support" in {
        Universe.createNew()
        val e1 = Select(0.1 -> 1, 0.2 -> 3, 0.3 -> 5, 0.5 -> 7)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(true)
        cc(e1).fullyRefined should be(true)
      }

      "a top-level component is atomic with finite support" in {
        Universe.createNew()
        val e1 = Binomial(10, 0.3)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(true)
        cc(e1).fullyRefined should be(true)
      }

      "a top-level component is continuous" in {
        Universe.createNew()
        val e1 = Normal(0, 1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(false)
        cc(e1).fullyRefined should be(false)
      }

      "a top-level component is discrete with infinite support" in {
        Universe.createNew()
        val e1 = Poisson(3)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(false)
        cc(e1).fullyRefined should be(false)
      }

      "the parents of a top-level component have finite support" in {
        Universe.createNew()
        val e1 = Select(0.1 -> 1, 0.2 -> 3, 0.3 -> 5, 0.5 -> 7)
        val e2 = Select(0.2 -> 3, 0.8 -> 4)
        val e3 = e1 ++ e2
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e3))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e3).fullyEnumerated should be(true)
        cc(e3).fullyRefined should be(true)
      }

      "the parent of a top-level component has infinite support" in {
        Universe.createNew()
        val e1 = Poisson(3)
        val e2 = Select(0.2 -> 3, 0.8 -> 4)
        val e3 = e1 ++ e2
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e3))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e3).fullyEnumerated should be(false)
        cc(e3).fullyRefined should be(false)
      }

      "a Chain's parent has infinite support and its result elements have finite support" in {
        Universe.createNew()
        val e1 = Normal(0, 1)
        val e2 = Chain(e1, (d: Double) => if(d < 0) Flip(0.2) else Flip(0.7))
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        val c2 = cc(e2)
        c2.fullyEnumerated should be(false)
        c2.fullyRefined should be(false)
        for((value, subproblem) <- c2.subproblems) {
          cc(subproblem.target).fullyEnumerated should be(true)
          subproblem.fullyRefined should be(true)
        }
      }

      "a Chain's result has infinite support" in {
        Universe.createNew()
        val e1 = Flip(0.3)
        val e2 = If(e1, Constant(0.0), Normal(0, 1))
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        val c2 = cc(e2)
        c2.fullyEnumerated should be(false)
        c2.fullyRefined should be(false)
        // The Constant(0.0) should be fully expanded, the Normal(0, 1) should not
        val prt = c2.subproblems(true)
        cc(prt.target).fullyEnumerated should be(true)
        prt.fullyRefined should be(true)
        val prf = c2.subproblems(false)
        cc(prf.target).fullyEnumerated should be(false)
        prf.fullyRefined should be(false)
      }

      "a Chain whose parent has finite support has fully expanded all of its subproblems" in {
        Universe.createNew()
        val e1 = Select(0.1 -> 1, 0.2 -> 3, 0.3 -> 5, 0.4 -> 7)
        val e2 = Chain(e1, (i: Int) => FromRange(0, i))
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        val c2 = cc(e2)
        c2.fullyEnumerated should be(true)
        c2.fullyRefined should be(true)
        for((value, subproblem) <- c2.subproblems) {
          cc(subproblem.target).fullyEnumerated should be(true)
          subproblem.fullyRefined should be(true)
        }
      }

      "a CompoundFlip has an infinite parent" in {
        Universe.createNew()
        val e1 = continuous.Uniform(0.0, 1.0)
        val e2 = Flip(e1)

        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e2).fullyEnumerated should be(true)
        cc(e2).fullyRefined should be(false)
      }

      "a CompoundSelect has an infinite parent" in {
        Universe.createNew()
        val e1 = continuous.Uniform(0.1, 0.7)
        val e2 = continuous.Uniform(0.2, 0.3)
        val e3 = Select((e1, "a"), (e2, "b"))

        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e3))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e3).fullyEnumerated should be(true)
        cc(e3).fullyRefined should be(false)
      }

      "a CompoundDist has an infinite parent but is fully enumerable" in {
        val e1 = continuous.Uniform(0.1, 0.7)
        val e2 = continuous.Uniform(0.2, 0.3)
        val e3 = Dist((e1, Flip(0.3)), (e2, Flip(0.5)))

        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e3))
        new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

        cc(e3).fullyEnumerated should be(true)
        cc(e3).fullyRefined should be(false)
      }
    }

    "memoize subproblems" in {
      Universe.createNew()
      val e1 = Flip(0.4)
      val chainFunc = (b: Boolean) => if(b) Select(0.1 -> 1, 0.9 -> 2) else Select(0.7 -> 3, 0.3 -> 4)
      val e2 = Chain(e1, chainFunc)
      val e3 = Chain(!e1, chainFunc)

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e2, e3))
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

      cc(e2).subproblems should equal(cc(e3).subproblems)
    }

    "add globals in Chains to the correct problem" in {
      Universe.createNew()
      val e1 = Chain(Flip(0.3), (b1: Boolean) => {
        val e2 = Constant(0)
        if(b1) Constant(1)
        else Chain(Flip(0.4), (b2: Boolean) => {
          if(b2) e2
          else e2.map(_ + 1)
        })
      })

      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      new BottomUpStrategy(pr, defaultRangeSizer, false, pr.targetComponents).execute()

      val c1 = cc(e1)
      // Should contain Constant(0) and Constant(1)
      c1.subproblems(true).components should have size 2
      // Should contain Constant(0), Flip(0.4), and Chain(Flip(0.4), ...)
      c1.subproblems(false).components should have size 3
    }
  }

  "A partial bottom-up strategy" when {
    // A simple recursive element; useful for testing lazy partial expansion
    // This explicitly does not use Chain function memoization
    def geometric(): Element[Int] = If(Flip(0.5), Constant(1), geometric().map(_ + 1))

    "called once" should {
      "produce the correct range" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new PartialBottomUpStrategy(3, pr, defaultRangeSizer, false, pr.targetComponents).execute()

        val c1 = cc(e1)
        c1.range.regularValues should equal(Set(1, 2, 3))
        c1.range.hasStar should be(true)
      }

      "mark components as fully refined and enumerated" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new PartialBottomUpStrategy(3, pr, defaultRangeSizer, false, pr.targetComponents).execute()

        val c1 = cc(e1).asInstanceOf[ChainComponent[Boolean, Int]]
        c1.fullyEnumerated should be(false)
        c1.fullyRefined should be(false)
        val prt = c1.subproblems(true)
        cc(prt.target).fullyEnumerated should be(true)
        prt.fullyRefined should be(true)
        val prf = c1.subproblems(false)
        cc(prf.target).fullyEnumerated should be(false)
        prf.fullyRefined should be(false)
      }

      "use the correct definition of depth with respect to globals" in {
        Universe.createNew()
        val e1 = geometric()
        val e2 = If(Flip(0.5), Constant(0), e1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        // Ensure that the global is added to the correct problem, even though this won't change the outcome of this test
        pr.add(e1)
        new PartialBottomUpStrategy(3, pr, defaultRangeSizer, false, pr.targetComponents).execute()

        // Because e1 is only used as the result of a Chain, decomposing e2 should count as incrementing the depth
        // Therefore, we should only recurse on subproblems of e1 twice
        cc(e1).range.regularValues should equal(Set(1, 2))
        cc(e2).range.regularValues should equal(Set(0, 1, 2))
      }
    }

    "called at increasing depth" should {
      "produce the correct range" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        val c1 = cc(e1)

        for(depth <- 0 to 10) {
          new PartialBottomUpStrategy(depth, pr, defaultRangeSizer, false, pr.targetComponents).execute()
          c1.range.hasStar should be(true)
          c1.range.regularValues should equal((1 to depth).toSet)
        }
      }

      "preserve only the relevant solutions" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new PartialBottomUpStrategy(1, pr, defaultRangeSizer, false, pr.targetComponents).execute()
        new ConstantStrategy(pr, structuredRaising, marginalVariableElimination).execute()

        val c1 = cc(e1).asInstanceOf[ChainComponent[Boolean, Int]]
        pr.solved should be(true)
        c1.subproblems(true).solved should be(true)
        c1.subproblems(false).solved should be(true)

        new PartialBottomUpStrategy(3, pr, defaultRangeSizer, false, pr.targetComponents).execute()

        pr.solved should be(false)
        // The first subproblem was fully refined so its solution should remain; the second subproblem was expanded
        // further so its solution should be removed
        c1.subproblems(true).solved should be(true)
        c1.subproblems(false).solved should be(false)
      }
    }
  }
}
