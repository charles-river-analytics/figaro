/*
 * MultiDecisionTest.scala  
 * Bayesian network examples tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.decision._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example
import com.cra.figaro.ndtest._

class MultiDecisionTest extends WordSpec with Matchers {

  def propmaker = (mv: Universe, e: Element[_]) => ProposalScheme.default(mv)

  "A multi decision network" should {
    "produce the correct decisions under variable elimination" taggedAs (Example) in {
      val result = doTest((e1: List[Element[Double]], e2: List[Decision[_, _]]) => MultiDecisionVariableElimination(e1, e2: _*))

      result(0) should equal(true)
      result(1) should equal(false)
      result(2) should equal(true)
      result(3) should equal(true)
      result(4) should equal(true)

    }

    "produce the correct decisions under importance sampling" taggedAs (Example) in {
      val result = doTest((e1: List[Element[Double]], e2: List[Decision[_, _]]) => MultiDecisionImportance(30000, e1, e2: _*))

      result(0) should equal(true)
      result(1) should equal(false)
      result(2) should equal(true)
      result(3) should equal(true)
      result(4) should equal(true)

    }

    "produce the correct decisions under Metropolis-Hastings" taggedAs (Example) in {
      val ndtest = new NDTest {
        override def oneTest = {

          val result = doTest((e1: List[Element[Double]], e2: List[Decision[_, _]]) =>
            MultiDecisionMetropolisHastings(300000, propmaker, 20000, e1, e2: _*))

          update(result(0), NDTest.BOOLEAN, "MHMulti-DecisionFound(-1)", true, .90)
          update(result(1), NDTest.BOOLEAN, "MHMulti-DecisionFound(0)", false, .90)
          update(result(2), NDTest.BOOLEAN, "MHMulti-DecisionFound(1)", true, .90)
          update(result(3), NDTest.BOOLEAN, "MHMulti-DecisionFound(2)", true, .90)
          update(result(4), NDTest.BOOLEAN, "MHMulti-DecisionTest(0)", true, .70)
        }
      }
      ndtest.run(10)

    }
  }

  def doTest(algorithmCreator: (List[Element[Double]], List[Decision[_, _]]) => MultiDecisionAlgorithm) = {
    Universe.createNew()
    val Market = Select(0.5 -> 0, 0.3 -> 1, 0.2 -> 2)
    val Test = Decision(Constant(0), List(true, false))
    val Cost = Apply(Test, (b: Boolean) => if (b) -1.0 else 0.0)

    val Survey = CPD(Test, Market,
      (false, 0) -> Constant(-1), (false, 1) -> Constant(-1), (false, 2) -> Constant(-1),
      (true, 0) -> Select(0.6 -> 0, 0.3 -> 1, 0.1 -> 2),
      (true, 1) -> Select(0.3 -> 0, 0.4 -> 1, 0.3 -> 2),
      (true, 2) -> Select(0.1 -> 0, 0.4 -> 1, 0.5 -> 2))

    val Found = Decision(Survey, List(true, false))
    def Value_fcn(f: Boolean, m: Int): Double = {
      if (f) {
        m match {
          case 0 => -7.0
          case 1 => 5.0
          case 2 => 20.0
        }
      } else {
        0.0
      }
    }
    val Value = Apply(Found, Market, Value_fcn)

    val alg = algorithmCreator(List(Value, Cost), List(Found, Test))
    alg.start()

    //          found.getPolicy(-1).value should equal(true)
    //          found.getPolicy(0).value should equal(false)
    //          found.getPolicy(1).value should equal(true)
    //          found.getPolicy(2).value should equal(true)
    //          test.getPolicy(0).value should equal(true)

    val result = List(Found.getPolicy(-1).value,
      Found.getPolicy(0).value,
      Found.getPolicy(1).value,
      Found.getPolicy(2).value,
      Test.getPolicy(0).value)

    alg.kill

    result
  }
}
