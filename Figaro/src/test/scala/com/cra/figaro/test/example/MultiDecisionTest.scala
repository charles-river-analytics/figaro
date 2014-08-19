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

class MultiDecisionTest extends WordSpec with Matchers {

  def propmaker = (mv: Universe, e: Element[_]) => ProposalScheme.default(mv)

  "A multi decision network" should {
    "produce the correct decisions under variable elimination" taggedAs (Example) in {
      test((e1: List[Element[Double]], e2: List[Decision[_, _]]) => MultiDecisionVariableElimination(e1, e2: _*))
    }

    "produce the correct decisions under importance sampling" taggedAs (Example) in {
      test((e1: List[Element[Double]], e2: List[Decision[_, _]]) => MultiDecisionImportance(30000, e1, e2: _*))
    }

    "produce the correct decisions under Metropolis-Hastings" taggedAs (Example) in {
      test((e1: List[Element[Double]], e2: List[Decision[_, _]]) =>
        MultiDecisionMetropolisHastings(200000, propmaker, 10000, e1, e2: _*))
    }
  }

  def test(algorithmCreator: (List[Element[Double]], List[Decision[_, _]]) => MultiDecisionAlgorithm): Unit = {
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

    Found.getPolicy(-1).value should equal(true)
    Found.getPolicy(0).value should equal(false)
    Found.getPolicy(1).value should equal(true)
    Found.getPolicy(2).value should equal(true)
    Test.getPolicy(0).value should equal(true)

    alg.kill
  }
}
