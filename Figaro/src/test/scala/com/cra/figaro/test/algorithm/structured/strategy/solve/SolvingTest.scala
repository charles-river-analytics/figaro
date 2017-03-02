/*
 * SolvingTest.scala
 * Tests for solving strategies.
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

import com.cra.figaro.algorithm.factored.factors.{Factor, Variable}
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.solver._
import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy
import com.cra.figaro.language.{Apply, Universe}
import com.cra.figaro.library.atomic.discrete.Uniform
import org.scalatest.{Matchers, WordSpec}

class SolvingTest extends WordSpec with Matchers {

  def solvingStrategy(problem: Problem, solver: Solver = marginalVariableElimination): SolvingStrategy = {
    new SolvingStrategy(problem) {
      override def eliminate(toEliminate: Set[Variable[_]], toPreserve: Set[Variable[_]], factors: List[Factor[Double]]): (List[Factor[Double]], Map[Variable[_], Factor[_]]) = {
        solver(problem, toEliminate, toPreserve, factors)
      }
    }
  }

  "A non-recursive solving strategy" when {
    "collecting factors" should {
      "collect the non-constraint factors for components" when {
        "given a component without subproblems" in {
          Universe.createNew()
          val e1 = Uniform(1, 2, 3)
          val e2 = Apply(e1, (i: Int) => i + 1)

          val cc = new ComponentCollection
          val pr = new Problem(cc, List(e2))
          pr.add(e1)
          val c1 = cc(e1)
          val c2 = cc(e2)
          c1.generateRange()
          c2.generateRange()

          val solve = solvingStrategy(pr)
          // One factor for each component
          solve.nonConstraintFactors() should have size 2
        }

        "given a component with range {*}" in {
          Universe.createNew()
          val e1 = Uniform(1, 2, 3)
          val e2 = Apply(e1, (i: Int) => i + 1)

          val cc = new ComponentCollection
          val pr = new Problem(cc, List(e2))
          pr.add(e1)
          val c1 = cc(e1)
          c1.generateRange()

          val solve = solvingStrategy(pr)
          // c2 has range {*}, so it should have no associated factors
          solve.nonConstraintFactors() should have size 1
        }
      }

      "collect the constraint factors for all components if the problem is not nested" in {
        Universe.createNew()
        val e1 = Uniform(1, 2, 3)
        val e2 = Apply(e1, (i: Int) => i + 1)
        e1.addCondition(_ > 1)
        e2.addConstraint(_.toDouble)

        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        pr.add(e1)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.generateRange()

        val solve = solvingStrategy(pr)
        solve.constraintFactors(Lower) should have size 2
        solve.constraintFactors(Upper) should have size 2
      }
    }

    "producing a solution" should {
      "mark the problem as solved and memoize the solution" in {
        Universe.createNew()
        val e1 = Uniform(1, 2, 3)
        val e2 = Apply(e1, (i: Int) => i + 1)

        val cc = new ComponentCollection
        val pr = new Problem(cc, List(e2))
        pr.add(e1)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.generateRange()

        val solve = solvingStrategy(pr)
        solve.execute()
        pr.solved should equal (true)
        pr.solution should not be empty
        pr.solution.flatMap(_.variables).toSet should equal(Set(c2.variable))
      }

      "memoize recording factors if applicable" in {
        Universe.createNew()
        val e1 = Uniform(1, 2, 3)
        val e2 = Apply(e1, (i: Int) => i + 1)

        val cc = new ComponentCollection
        val pr = new Problem(cc, List())
        pr.add(e1)
        pr.add(e2)
        val c1 = cc(e1)
        val c2 = cc(e2)
        c1.generateRange()
        c2.generateRange()

        val solve = solvingStrategy(pr, mpeVariableElimination)
        solve.execute()
        pr.solved should equal (true)
        pr.recordingFactors.keySet should equal(Set(c1.variable, c2.variable))
      }
    }
  }
}
