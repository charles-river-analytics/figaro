/*
 * BacktrackingStrategyTest.scala
 * Tests for backtracking strategies.
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

import com.cra.figaro.algorithm.factored.factors.DenseFactor
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.structured.strategy.refine._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.range.SamplingRanger
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous
import com.cra.figaro.library.atomic.continuous.{Beta, Normal}
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.compound.If
import org.scalatest.{Matchers, WordSpec}

class BacktrackingStrategyTest extends WordSpec with Matchers {
  "A complete backtracking strategy" should {
    "create ranges for all components" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = If(e1, Constant(0), discrete.Uniform(2, 3, 4))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e2))
      new BacktrackingStrategy(pr, pr.targetComponents).execute()

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

    "create non-constraint factors for all components" in {
      Universe.createNew()
      val e1 = Flip(0.3)
      val e2 = If(e1, Constant(0), discrete.Uniform(2, 3, 4))
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e2))
      new BacktrackingStrategy(pr, pr.targetComponents).execute()

      val c1 = cc(e1)
      val c2 = cc(e2)
      // Each component has 1 factor
      // Chain components additionally have 1 factor per subproblem
      c1.nonConstraintFactors() should have size 1
      c2.nonConstraintFactors() should have size 3

      val c1t = cc(c2.subproblems(true).target)
      val c1f = cc(c2.subproblems(false).target)
      c1t.nonConstraintFactors() should have size 1
      c1f.nonConstraintFactors() should have size 1
    }

    "create upper and lower bound constraint factors for top-level components" in {
      Universe.createNew()
      val e1 = discrete.Uniform(1, 2, 3)
      e1.addConstraint((i: Int) => 1.0 / i)
      val cc = new ComponentCollection
      val pr = new Problem(cc, List(e1))
      new BacktrackingStrategy(pr, pr.targetComponents).execute()

      val c1 = cc(e1)
      c1.constraintFactors(Lower) should have size 1
      c1.constraintFactors(Upper) should have size 1
    }

    "conditionally make parameterized factors" when {
      "parameterized is set to false" in {
        Universe.createNew()
        val e1 = Beta(1, 2)
        val e2 = Flip(e1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1, e2))
        val c1 = cc(e1)
        c1.ranger.asInstanceOf[SamplingRanger[Double]].samplesPerIteration = 25
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        // Factor for e1 should contain sampled values
        // Its size should be the size of the range of the component
        val List(f1) = cc(e1).nonConstraintFactors()
        f1 should have size 25
        // Factor for e2 should encode the conditional probability distribution
        // Its size should be the product of the sizes of the ranges of the components
        val List(f2) = cc(e2).nonConstraintFactors()
        f2 should have size 50
      }

      // Ranging in SFI doesn't currently support parameterized = true
      /*"parameterized is set to true" in {
        Universe.createNew()
        val e1 = Beta(1, 2)
        val e2 = Flip(e1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1, e2))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()
      }*/
    }

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
      new BacktrackingStrategy(spr, spr.targetComponents).execute()
      new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
      // This should not get rid of the solution
      new BacktrackingStrategy(pr, pr.targetComponents).execute()

      spr.solved should be(true)
      val solution = spr.solution.head
      val range = cc(spr.target).variable.range
      solution.get(List(range.indexOf(Regular(1)))) should be(0.1 +- 0.000000001)
      solution.get(List(range.indexOf(Regular(2)))) should be(0.9 +- 0.000000001)
    }

    "recursively mark problems as unsolved" when {
      // A "solution" which we will artificially place in problems to make sure it gets removed
      val dummySolution = List(new DenseFactor[Double](List(), List()))

      "executed on a top-level problem with no subproblems" in {
        Universe.createNew()
        val e1 = Flip(0.3)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        pr.solved = true
        pr.solution = dummySolution
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents, 0).execute()

        pr.solved = true
        pr.solution = dummySolution
        for((_, subproblem) <- cc(e2).subproblems) {
          subproblem.solved = true
          subproblem.solution = dummySolution
        }
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents, 0).execute()

        // We don't mark the nested problems as solved because this test is to ensure that we recursively go up the
        // problem tree without stopping at unsolved components
        pr.solved = true
        pr.solution = dummySolution
        new BacktrackingStrategy(cc(e2).subproblems(true), pr.targetComponents).execute()

        pr.solved should be(false)
        pr.solution should be(empty)
      }
    }

    "correctly mark components as fully enumerated and refined" when {
      "a top-level component has finite support" in {
        Universe.createNew()
        val e1 = Select(0.1 -> 1, 0.2 -> 3, 0.3 -> 5, 0.5 -> 7)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(true)
        cc(e1).fullyRefined should be(true)
      }

      "a top-level component is atomic with finite support" in {
        Universe.createNew()
        val e1 = Binomial(10, 0.3)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(true)
        cc(e1).fullyRefined should be(true)
      }

      "a top-level component is continuous" in {
        Universe.createNew()
        val e1 = Normal(0, 1)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        cc(e1).fullyEnumerated should be(false)
        cc(e1).fullyRefined should be(false)
      }

      "a top-level component is discrete with infinite support" in {
        Universe.createNew()
        val e1 = Poisson(3)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        cc(e3).fullyEnumerated should be(false)
        cc(e3).fullyRefined should be(false)
      }

      "a Chain's parent has infinite support and its result elements have finite support" in {
        Universe.createNew()
        val e1 = Normal(0, 1)
        val e2 = Chain(e1, (d: Double) => if(d < 0) Flip(0.2) else Flip(0.7))
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        val c2 = cc(e2)
        c2.fullyEnumerated should be(false)
        c2.fullyRefined should be(false)
        for((_, subproblem) <- c2.subproblems) {
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
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        val c2 = cc(e2)
        c2.fullyEnumerated should be(true)
        c2.fullyRefined should be(true)
        for((_, subproblem) <- c2.subproblems) {
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
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

        cc(e3).fullyEnumerated should be(true)
        cc(e3).fullyRefined should be(false)
      }

      "a CompoundDist has an infinite parent but is fully enumerable" in {
        Universe.createNew()
        val e1 = continuous.Uniform(0.1, 0.7)
        val e2 = continuous.Uniform(0.2, 0.3)
        val e3 = Dist((e1, Flip(0.3)), (e2, Flip(0.5)))

        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e3))
        new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
      new BacktrackingStrategy(pr, pr.targetComponents).execute()

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
      new BacktrackingStrategy(pr, pr.targetComponents).execute()

      val c1 = cc(e1)
      // Should contain Constant(0) and Constant(1)
      c1.subproblems(true).components should have size 2
      // Should contain Constant(0), Flip(0.4), and Chain(Flip(0.4), ...)
      c1.subproblems(false).components should have size 3
    }
  }

  // A simple recursive element; useful for testing lazy partial expansion
  // This explicitly does not use Chain function memoization
  def geometric(): Element[Int] = If(Flip(0.5), Constant(1), geometric().map(_ + 1))
  // This explicitly uses Chain function memoization
  def memoGeometric(): Element[Int] = Chain(Flip(0.5), memoGeometricFunc)
  // Memoization only works when we use the same function instance at each recursion (i.e. we need reference equality)
  val memoGeometricFunc = (b: Boolean) => {
    if(b) Constant(1)
    else memoGeometric().map(_ + 1)
  }

  "A partial backtracking strategy" when {
    "called once" should {
      "produce the correct range" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents, 3).execute()

        val c1 = cc(e1)
        c1.range.regularValues should equal(Set(1, 2, 3))
        c1.range.hasStar should be(true)
      }

      "mark components as fully refined and enumerated" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents, 3).execute()

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
        new BacktrackingStrategy(pr, pr.targetComponents, 3).execute()

        // Because e1 is only used as the result of a Chain, decomposing e2 should count as incrementing the depth
        // Therefore, we should only recurse on subproblems of e1 twice
        cc(e1).range.regularValues should equal(Set(1, 2))
        cc(e2).range.regularValues should equal(Set(0, 1, 2))
      }

      "correctly backtrack when a component is processed multiple times at different depths" in {
        Universe.createNew()
        val g = (i: Int) => Select(0.5 -> i, 0.5 -> (i + 1))
        val e1 = Flip(0.2)
        val e2 = Chain(e1, (b: Boolean) => if(b) Constant(1) else Chain(g(0), g))
        val e3 = Chain(e2, g)
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e3))
        pr.add(e1)
        pr.add(e2)
        val c2 = cc(e2)
        val c3 = cc(e3)
        val strategy = new BacktrackingStrategy(pr, pr.targetComponents, 1)
        strategy.execute()

        // Expanding this problem proceeds depth-first from e3. It first goes through the Chain parents, which means
        // that e2 is processed before e3. At this point, the value set of e2 is {1,*}, and subproblems for g with
        // parent values 0 and 1 have been created but not visited. When e3 gets visited, the subproblem of g with
        // parent value 1 is visited at greater depth. This forces an update to e2, addint 2 to its range. Now, the
        // subproblem of g with parent value 2 gets expanded and visited, too. This does not force an update to e2
        // because e2 does not use the subproblem of g corresponding to the parent value 2. So, the end result is that
        // the only subproblem not fully refined is the subproblem of g with parent value 0.
        c2.range.hasStar should be(true)
        c2.range.regularValues should equal(Set(1, 2))
        c3.range.hasStar should be(true)
        c3.range.regularValues should equal(Set(1, 2, 3))
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
          new BacktrackingStrategy(pr, pr.targetComponents, depth).execute()
          c1.range.hasStar should be(true)
          c1.range.regularValues should equal((1 to depth).toSet)
        }
      }

      "preserve only the relevant solutions" in {
        Universe.createNew()
        val e1 = geometric()
        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents, 1).execute()
        new ConstantStrategy(pr, structuredRaising, marginalVariableElimination).execute()

        val c1 = cc(e1).asInstanceOf[ChainComponent[Boolean, Int]]
        pr.solved should be(true)
        c1.subproblems(true).solved should be(true)
        c1.subproblems(false).solved should be(true)

        new BacktrackingStrategy(pr, pr.targetComponents, 3).execute()

        pr.solved should be(false)
        // The first subproblem was fully refined so its solution should remain; the second subproblem was expanded
        // further so its solution should be removed
        c1.subproblems(true).solved should be(true)
        c1.subproblems(false).solved should be(false)
      }
    }

    "given a recursive model that uses the same function at each recursion" should {
      "reuse the non-recursive part of the model, but produce a different subproblem for the recursive part" in {
        Universe.createNew()
        val e1 = memoGeometric()
        val cc = new RecursiveComponentCollection
        val pr = new Problem(cc, List(e1))
        new BacktrackingStrategy(pr, pr.targetComponents, 2).execute()

        val c1 = cc(e1).asInstanceOf[ChainComponent[Boolean, Int]]
        val nestedc1 = c1.subproblems(false).components.collectFirst{
          case chainComp: ChainComponent[_, _] => chainComp
        }.get.asInstanceOf[ChainComponent[Boolean, Int]]

        // Sanity check; verify that the chain functions are the same (which normally induces memoization)
        c1.chain.chainFunction should equal(nestedc1.chain.chainFunction)
        // The subproblem corresponding to parent value true is non-recursive, but corresponding to false is recursive
        nestedc1.subproblems(true) should equal(c1.subproblems(true))
        nestedc1.subproblems(false) should not equal c1.subproblems(false)
      }
    }
  }
}
