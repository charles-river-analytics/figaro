/*
 * RaisingTest.scala
 * Tests for raising strategies.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Jan 23, 2017
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.structured.strategy.solve

import com.cra.figaro.algorithm.factored.factors.{Factor, InternalChainVariable}
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve._
import com.cra.figaro.algorithm.structured.{ComponentCollection, Problem}
import com.cra.figaro.language._
import org.scalatest.{Matchers, WordSpec}

class RaisingTest extends WordSpec with Matchers {

  "A raising strategy" when {
    "eliminating variables" should {
      "eliminate intermediates and remove them from the collection" in {
        val e1 = Flip(0.5)
        val e2 = Constant(0)
        val e3 = Chain(e1, (b: Boolean) => if(b) e2 else Select(0.1 -> 1, 0.9 -> 2))
        val cc = new ComponentCollection()
        val pr = new Problem(cc, List(e3))
        pr.add(e1)
        pr.add(e2)

        val c1 = cc(e1)
        c1.generateRange()
        val c3 = cc(e3)
        c3.expand()
        for((parentValue, spr) <- c3.subproblems) {
          cc(spr.target).generateRange()
        }
        c3.generateRange()

        val strategy = new ConstantStrategy(pr, structuredRaising, marginalVariableElimination) {
          // Make nonConstraintFactors a lazy val so we can look at cc.intermediates
          override lazy val nonConstraintFactors: List[Factor[Double]] = super.nonConstraintFactors()
        }
        strategy.nonConstraintFactors
        cc.intermediates shouldNot be(empty)
        val intermediates = cc.intermediates
        strategy.execute()
        // Executing the strategy should clear the intermediates
        cc.intermediates should be(empty)
        // The solution should contain no intermediates
        pr.solution.flatMap(_.variables).filter(intermediates) should be(empty)
      }

      "preserve globals" in {
        val e1 = Flip(0.5)
        val e2 = Select(0.1 -> 1, 0.9 -> 2)
        val e3 = Select(0.3 -> 3, 0.7 -> 4)
        val e4 = Chain(e1, (b: Boolean) => if(b) e2 ++ e3 else Constant(0))
        val cc = new ComponentCollection()
        val pr = new Problem(cc, List(e4))
        pr.add(e1)
        // e2 is a global, e3 is not
        pr.add(e2)

        val c1 = cc(e1)
        val c2 = cc(e2)
        val c4 = cc(e4)
        c1.generateRange()
        c2.generateRange()
        c4.expand()
        c4.subproblems(true).add(e3)
        val c3 = cc(e3)
        c3.generateRange()
        for((parentValue, spr) <- c4.subproblems) {
          cc(spr.target).generateRange()
        }
        c4.generateRange()

        val spr = c4.subproblems(true)
        val strategy = new ConstantStrategy(spr, structuredRaising, marginalVariableElimination)
        strategy.execute()
        spr.globals should equal(Set(c2, cc(spr.target)))
      }
    }

    "collecting subproblem factors" should {
      "always use the solved factors if the subproblem is already solved" when {
        "the raising criteria evaluates to false" in {
          Universe.createNew()
          val e1 = Flip(0.3)
          val e2 = Constant(0)
          val e3 = e2.map(_ + 1)
          val e4 = Select(0.2 -> 1, 0.8 -> 2)
          val e5 = e4.map(_ + 2)
          val e6 = Chain(e1, (b: Boolean) => if(b) e3 else e5)

          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e6))
          pr.add(e1)
          val c1 = cc(e1)
          c1.generateRange()
          val c6 = cc(e6)
          c6.expand()
          c6.subproblems(true).add(e2)
          c6.subproblems(false).add(e4)
          val c2 = cc(e2)
          val c3 = cc(e3)
          val c4 = cc(e4)
          val c5 = cc(e5)
          c2.generateRange()
          c3.generateRange()
          c4.generateRange()
          c5.generateRange()
          c6.generateRange()
          c6.subproblems.values.foreach(new ConstantStrategy(_, structuredRaising, marginalVariableElimination).execute())

          val strategy = new ConstantStrategy(pr, structuredRaising, marginalVariableElimination)
          val subproblemFactors = strategy.subproblemNonConstraintFactors(c6)
          // The factors should be eliminated to just the subproblem targets
          subproblemFactors(true) should have size 1
          subproblemFactors(false) should have size 1
          val List(c3Factor) = subproblemFactors(true)
          val List(c5Factor) = subproblemFactors(false)
          c3Factor.variables should equal(List(c3.variable))
          c5Factor.variables should equal(List(c5.variable))
        }

        "the raising criteria evaluates to true" in {
          Universe.createNew()
          val e1 = Flip(0.3)
          val e2 = Constant(0)
          val e3 = e2.map(_ + 1)
          val e4 = Select(0.2 -> 1, 0.8 -> 2)
          val e5 = e4.map(_ + 2)
          val e6 = Chain(e1, (b: Boolean) => if(b) e3 else e5)

          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e6))
          pr.add(e1)
          val c1 = cc(e1)
          c1.generateRange()
          val c6 = cc(e6)
          c6.expand()
          c6.subproblems(true).add(e2)
          c6.subproblems(false).add(e4)
          val c2 = cc(e2)
          val c3 = cc(e3)
          val c4 = cc(e4)
          val c5 = cc(e5)
          c2.generateRange()
          c3.generateRange()
          c4.generateRange()
          c5.generateRange()
          c6.generateRange()
          c6.subproblems.values.foreach(new ConstantStrategy(_, structuredRaising, marginalVariableElimination).execute())

          // The only difference between this test and the one above is that we use flat instead of structured raising
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          val subproblemFactors = strategy.subproblemNonConstraintFactors(c6)
          // The factors should be eliminated to just the subproblem targets
          subproblemFactors(true) should have size 1
          subproblemFactors(false) should have size 1
          val List(c3Factor) = subproblemFactors(true)
          val List(c5Factor) = subproblemFactors(false)
          c3Factor.variables should equal(List(c3.variable))
          c5Factor.variables should equal(List(c5.variable))
        }
      }

      "raise or solve according to the decision function if the subproblem is unsolved" when {
        "the raising criteria evaluates to false" in {
          Universe.createNew()
          val e1 = Flip(0.3)
          val e2 = Constant(0)
          val e3 = e2.map(_ + 1)
          val e4 = Select(0.2 -> 1, 0.8 -> 2)
          val e5 = e4.map(_ + 2)
          val e6 = Chain(e1, (b: Boolean) => if(b) e3 else e5)

          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e6))
          pr.add(e1)
          val c1 = cc(e1)
          c1.generateRange()
          val c6 = cc(e6)
          c6.expand()
          c6.subproblems(true).add(e2)
          c6.subproblems(false).add(e4)
          val c2 = cc(e2)
          val c3 = cc(e3)
          val c4 = cc(e4)
          val c5 = cc(e5)
          c2.generateRange()
          c3.generateRange()
          c4.generateRange()
          c5.generateRange()
          c6.generateRange()
          // We don't explicitly solve the subproblems here

          val strategy = new ConstantStrategy(pr, structuredRaising, marginalVariableElimination)
          val subproblemFactors = strategy.subproblemNonConstraintFactors(c6)
          // The factors should be eliminated to just the subproblem targets
          subproblemFactors(true) should have size 1
          subproblemFactors(false) should have size 1
          val List(c3Factor) = subproblemFactors(true)
          val List(c5Factor) = subproblemFactors(false)
          c3Factor.variables should equal(List(c3.variable))
          c5Factor.variables should equal(List(c5.variable))
        }

        "the raising criteria evaluates to true" in {
          Universe.createNew()
          val e1 = Flip(0.3)
          val e2 = Constant(0)
          val e3 = e2.map(_ + 1)
          val e4 = Select(0.2 -> 1, 0.8 -> 2)
          val e5 = e4.map(_ + 2)
          val e6 = Chain(e1, (b: Boolean) => if(b) e3 else e5)

          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e6))
          pr.add(e1)
          val c1 = cc(e1)
          c1.generateRange()
          val c6 = cc(e6)
          c6.expand()
          c6.subproblems(true).add(e2)
          c6.subproblems(false).add(e4)
          val c2 = cc(e2)
          val c3 = cc(e3)
          val c4 = cc(e4)
          val c5 = cc(e5)
          c2.generateRange()
          c3.generateRange()
          c4.generateRange()
          c5.generateRange()
          c6.generateRange()
          // We don't explicitly solve the subproblems here

          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          val subproblemFactors = strategy.subproblemNonConstraintFactors(c6)
          // Because we used a flat strategy and the subproblems were not solved beforehand, the factors should have
          // been raised without elimination
          subproblemFactors(true) should have size 2
          subproblemFactors(false) should have size 2
          subproblemFactors(true).flatMap(_.variables).toSet should equal(Set(c2.variable, c3.variable))
          subproblemFactors(false).flatMap(_.variables).toSet should equal(Set(c4.variable, c5.variable))
        }
      }
    }

    "collecting Chain factors" should {
      "return an empty list when the range of the Chain is {*}" in {
        val e1 = Flip(0.5)
        val e2 = Chain(e1, (b: Boolean) => if(b) Constant(0) else Select(0.1 -> 1, 0.9 -> 2))
        val cc = new ComponentCollection()
        val pr = new Problem(cc, List(e2))
        pr.add(e1)

        val c1 = cc(e1)
        c1.generateRange()
        val c2 = cc(e2)
        c2.expand()
        // Note: we don't generate ranges for subproblems
        c2.generateRange()

        val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
        strategy.chainNonConstraintFactors(c2) should be(empty)
      }

      "correctly choose to apply the new Chain method" when {
        "all subproblems are solved and contain no globals" in {
          val e1 = Flip(0.5)
          val e2 = Chain(e1, (b: Boolean) => if(b) Constant(0) else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e2))
          pr.add(e1)

          val c1 = cc(e1)
          c1.generateRange()
          val c2 = cc(e2)
          c2.expand()
          for(spr <- c2.subproblems.values) {
            cc(spr.target).generateRange()
            new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
          }
          c2.generateRange()

          c2.allSubproblemsEliminatedCompletely should equal(true)
          cc.useSingleChainFactor = true
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          // This should return a single compact factor over the Chain and its parent
          val List(c2Factor) = strategy.chainNonConstraintFactors(c2)
          c2Factor.variables.toSet should equal(Set(c1.variable, c2.variable))
        }

        "the option is disabled" in {
          val e1 = Flip(0.5)
          val e2 = Chain(e1, (b: Boolean) => if(b) Constant(0) else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e2))
          pr.add(e1)

          val c1 = cc(e1)
          c1.generateRange()
          val c2 = cc(e2)
          c2.expand()
          for(spr <- c2.subproblems.values) {
            cc(spr.target).generateRange()
            new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
          }
          c2.generateRange()

          c2.allSubproblemsEliminatedCompletely should equal(true)
          cc.useSingleChainFactor = false
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          // This should return multiple factors instead of a single compact factor
          strategy.chainNonConstraintFactors(c2).length should be > 1
        }

        "all subproblems are solved but one or more outcomes are globals" in {
          val e1 = Flip(0.5)
          val e2 = Constant(0)
          val e3 = Chain(e1, (b: Boolean) => if(b) e2 else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e3))
          pr.add(e1)
          // Chain outcome e2 is global
          pr.add(e2)

          val c1 = cc(e1)
          c1.generateRange()
          val c3 = cc(e3)
          c3.expand()
          // This includes generating range for e2
          for(spr <- c3.subproblems.values) {
            cc(spr.target).generateRange()
            new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
          }
          c3.generateRange()

          c3.allSubproblemsEliminatedCompletely should equal(false)
          cc.useSingleChainFactor = true
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          // This should return multiple factors instead of a single compact factor
          strategy.chainNonConstraintFactors(c3).length should be > 1
        }

        "all subproblems are solved but solutions contain globals" in {
          val e1 = Flip(0.5)
          val e2 = Constant(0)
          val e3 = Chain(e1, (b: Boolean) => if(b) e2.map(_ + 1) else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e3))
          pr.add(e1)
          // Chain outcome e2 is not global, but it depends on one
          pr.add(e2)

          val c1 = cc(e1)
          c1.generateRange()
          val c2 = cc(e2)
          c2.generateRange()
          val c3 = cc(e3)
          c3.expand()
          for(spr <- c3.subproblems.values) {
            cc(spr.target).generateRange()
            new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
          }
          c3.generateRange()

          c3.allSubproblemsEliminatedCompletely should equal(false)
          cc.useSingleChainFactor = true
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          // This should return multiple factors instead of a single compact factor
          strategy.chainNonConstraintFactors(c3).length should be > 1
        }

        "any unsolved subproblems were raised" in {
          val e1 = Flip(0.5)
          val e2 = Chain(e1, (b: Boolean) => if(b) Constant(0) else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e2))
          pr.add(e1)

          val c1 = cc(e1)
          c1.generateRange()
          val c2 = cc(e2)
          c2.expand()
          for((parentValue, spr) <- c2.subproblems) {
            cc(spr.target).generateRange()
            // Only solve one of the subproblems
            if(parentValue) new ConstantStrategy(spr, structuredRaising, marginalVariableElimination).execute()
          }
          c2.generateRange()

          c2.allSubproblemsEliminatedCompletely should equal(false)
          cc.useSingleChainFactor = true
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          // This should return multiple factors instead of a single compact factor
          strategy.chainNonConstraintFactors(c2).length should be > 1
        }
      }

      "replace the variable in the raised factors with the actual variable in the Chain factors" when {
        "the subproblem is solved" in {
          val e1 = Flip(0.5)
          val e2 = Constant(0)
          val e3 = Chain(e1, (b: Boolean) => if(b) e2 else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e3))
          pr.add(e1)
          pr.add(e2)

          val c1 = cc(e1)
          c1.generateRange()
          val c2 = cc(e2)
          val c3 = cc(e3)
          c3.expand()
          for((parentValue, spr) <- c3.subproblems) {
            cc(spr.target).generateRange()
          }
          c3.generateRange()

          // Use structuredRaising so we solve subproblems first
          val strategy = new ConstantStrategy(pr, structuredRaising, marginalVariableElimination)
          // Collect all variables in the factors
          val allVariables = strategy.chainNonConstraintFactors(c3).flatMap(_.variables).toSet
          // Separate into internal and regular variables
          val (internal, regular) = allVariables.partition(_.isInstanceOf[InternalChainVariable[_]])
          // The only internal variable should be the tuple variable in the Chain factor
          internal should have size 1
          // For globals, the actual variable used should be the same
          c3.actualSubproblemVariables(true) should be theSameInstanceAs c2.variable
          // For non-globals, a different variable should be used
          c3.actualSubproblemVariables(false) shouldNot be theSameInstanceAs cc(c3.subproblems(false).target).variable
          // There should be four non-internal variables: the parent, the two outcome variables, and the Chain itself
          regular should equal(Set(c1.variable, c2.variable, c3.actualSubproblemVariables(false), c3.variable))
        }

        "the subproblem is raised" in {
          val e1 = Flip(0.5)
          val e2 = Constant(0)
          val e3 = Chain(e1, (b: Boolean) => if(b) e2 else Select(0.1 -> 1, 0.9 -> 2))
          val cc = new ComponentCollection()
          val pr = new Problem(cc, List(e3))
          pr.add(e1)
          pr.add(e2)

          val c1 = cc(e1)
          c1.generateRange()
          val c2 = cc(e2)
          val c3 = cc(e3)
          c3.expand()
          for((parentValue, spr) <- c3.subproblems) {
            cc(spr.target).generateRange()
          }
          c3.generateRange()

          // Use flatRaising so we don't solve subproblems
          val strategy = new ConstantStrategy(pr, flatRaising, marginalVariableElimination)
          // Collect all variables in the factors
          val allVariables = strategy.chainNonConstraintFactors(c3).flatMap(_.variables).toSet
          // Separate into internal and regular variables
          val (internal, regular) = allVariables.partition(_.isInstanceOf[InternalChainVariable[_]])
          // The only internal variable should be the tuple variable in the Chain factor
          internal should have size 1
          // For globals, the actual variable used should be the same
          c3.actualSubproblemVariables(true) should be theSameInstanceAs c2.variable
          // For non-globals, a different variable should be used
          c3.actualSubproblemVariables(false) shouldNot be theSameInstanceAs cc(c3.subproblems(false).target).variable
          // There should be four non-internal variables: the parent, the two outcome variables, and the Chain itself
          regular should equal(Set(c1.variable, c2.variable, c3.actualSubproblemVariables(false), c3.variable))
        }
      }
    }
  }
}
