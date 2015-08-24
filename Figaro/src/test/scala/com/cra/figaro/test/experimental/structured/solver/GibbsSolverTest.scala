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
package com.cra.figaro.test.experimental.structured.solver

import org.scalatest.{ Matchers, WordSpec }

import com.cra.figaro.experimental.factored.BlockSampler
import com.cra.figaro.experimental.structured.{ ComponentCollection, Problem }
import com.cra.figaro.experimental.structured.solver.GibbsSolver
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.If

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
}
