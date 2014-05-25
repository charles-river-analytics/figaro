/*
 * DecisionUtilTest.scala   
 * Variable elimination tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.decision

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.decision._
import com.cra.figaro.library.decision.DecisionUtil._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.util._
import com.cra.figaro.test._
import scala.collection.mutable.Map

class DecisionUtilTest extends WordSpec with Matchers {

  "DecisionUtil" should {

    "return the correct decision order" in {
      val (univ, dec, util, rest) = genNetwork()
      val order = getDecisionOrder(dec, univ)
      order should have size (2)
      order(0) should have size (2)
      order(1) should have size (1)
      assert(order(0) contains dec(0))
      assert(order(0) contains dec(2))
      assert(order(1) contains dec(1))
    }

    "return the correct predecessor decisions" in {
      val (univ, dec, util, rest) = genNetwork()
      val order = getDecisionOrder(dec, univ)
      val pred_d2 = getPredDecisions(dec(1), order, univ)
      val pred_d3 = getPredDecisions(dec(2), order, univ)
      assert(pred_d2 contains dec(0))
      pred_d3 should be('empty)
    }

    "return the correct successor decisions" in {
      val (univ, dec, util, rest) = genNetwork()
      val order = getDecisionOrder(dec, univ)
      val succ_d1 = getSuccDecisions(dec(0), order, univ)
      val succ_d2 = getSuccDecisions(dec(1), order, univ)
      val succ_d3 = getSuccDecisions(dec(2), order, univ)
      assert(succ_d1 contains dec(1))
      succ_d1 should have size (1)
      succ_d2 should be('empty)
      succ_d3 should be('empty)
    }

    "return the relevent utilities" in {
      val (univ, dec, util, rest) = genNetwork()
      val order = getDecisionOrder(dec, univ)
      val util_d1 = getReleventUtil(dec(0), util, order, univ)
      val util_d2 = getReleventUtil(dec(1), util, order, univ)
      val util_d3 = getReleventUtil(dec(2), util, order, univ)
      assert(util_d1 contains util(0))
      util_d1 should have size (1)
      assert(util_d2 contains util(1))
      util_d2 should have size (1)
      assert(util_d3 contains util(2))
      util_d3 should have size (1)
    }

    "return only the nearest successor decisions" in {
      val (univ, dec, util, rest) = genNetwork2()
      val order = getDecisionOrder(dec, univ)
      univ.clearTemporaries()
      val elems_d1 = getElemsForDecision(dec(0), util, order, univ)
      val elems_d2 = getElemsForDecision(dec(1), util, order, univ)
      val elems_d3 = getElemsForDecision(dec(2), util, order, univ)
      val elems_d4 = getElemsForDecision(dec(3), util, order, univ)
      assert(elems_d1.contains(dec(2)) == false && elems_d1.contains(dec(3)) == false)
      assert(elems_d2.contains(dec(3)) == false)
      assert(elems_d3.contains(dec(0)) == false)
      assert(elems_d4.contains(dec(0)) == false && elems_d4.contains(dec(1)) == false)

    }

    "return the correct elements for a decision" in {
      val (univ, dec, util, rest) = genNetwork()
      val order = getDecisionOrder(dec, univ)
      univ.clearTemporaries()
      val elems_d1 = getElemsForDecision(dec(0), util, order, univ)
      val elems_d2 = getElemsForDecision(dec(1), util, order, univ)
      val elems_d3 = getElemsForDecision(dec(2), util, order, univ)
      assert(rest(0).diff(elems_d1).nonEmpty == false)
      assert(rest(1).diff(elems_d2).nonEmpty == false)
      assert(rest(2).diff(elems_d3).nonEmpty == false)
    }

  }

  def genNetwork(): (Universe, List[Element[_]], List[Element[_]], List[List[Element[_]]]) = {

    val U = Universe.createNew()
    val f1 = Flip(0.1)
    val d1 = Decision(f1, 0.1 to 0.5 by 0.1)
    val u1 = Apply(d1, (p: Double) => p)
    val f2 = Flip(d1)
    val f3 = Flip(0.2)
    val f2f3 = ^^(f2, f3)
    val d2 = Decision(f2f3, 0.1 to 0.5 by 0.1)
    val f4 = Flip(d2)
    val f5 = Flip(0.3)
    val u2 = CPD(f4, f5, (true, true) -> Constant(1.0), (false, true) -> Constant(1.0),
      (true, false) -> Constant(1.0), (false, false) -> Constant(1.0))
    val d3 = Decision(Constant(0), 0.1 to 0.5 by 0.1)
    val f6 = Flip(d3)
    val u3 = CPD(f5, f6, (true, true) -> Constant(1.0), (false, true) -> Constant(1.0),
      (true, false) -> Constant(1.0), (false, false) -> Constant(1.0))
    val elems_d1_list = List(f1, d1, u1, f2, f3, f2f3, d2)
    val elems_d2_list = List(d1, f2, f3, f2f3, d2, f4, f5, u2)
    val elems_d3_list = List(d3, f6, u3, f5)
    (U, List(d1, d2, d3), List(u1, u2, u3), List(elems_d1_list, elems_d2_list, elems_d3_list))

  }

  def genNetwork2(): (Universe, List[Element[_]], List[Element[_]], List[List[Element[_]]]) = {

    val U = Universe.createNew()

    val f1 = Flip(0.5)
    val d1 = Decision(f1, 0.1 to 0.5 by 0.1)("d1", U)
    val u1 = Apply(f1, d1, (f: Boolean, p: Double) => if (f == true) .4 - 5 else p - .5)
    val f2 = Flip(d1)
    val d2 = Decision(f2, 0.1 to 0.5 by 0.1)("d2", U)
    val u2 = Apply(f2, d2, (f: Boolean, p: Double) => if (f == true) .4 - p else p - .4)
    val f3 = Flip(d2)
    val d3 = Decision(f3, 0.1 to 0.5 by 0.1)("d3", U)
    val u3 = Apply(f3, d3, (f: Boolean, p: Double) => if (f == true) p - .2 else .2 - p)
    val f4 = Flip(d3)
    val d4 = Decision(f4, 0.1 to 0.5 by 0.1)("d4", U)
    val u4 = Apply(f4, d4, (f: Boolean, p: Double) => if (f == true) p - .1 else .1 - p)

    val elems_d1_list = List(f1, d1, u1, f2, d2)
    val elems_d2_list = List(d1, f2, d2, u2, f3, d3)
    val elems_d3_list = List(d2, f3, d3, u3, f4, d4)
    val elems_d4_list = List(d3, f4, d4, u4)

    (U, List(d1, d2, d3, d4), List(u1, u2, u3, u4), List(elems_d1_list, elems_d2_list, elems_d3_list, elems_d4_list))

  }

}













