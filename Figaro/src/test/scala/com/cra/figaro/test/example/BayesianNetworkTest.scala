/*
 * BayesianNetworkTest.scala 
 * Bayesian network examples tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.test._

class BayesianNetworkTest extends WordSpec with ShouldMatchers {
  "A simple Bayesian network" should {
    "produce the correct probability under variable elimination" taggedAs (ExampleTest) in {
      test((e1: Element[Boolean], e2: Element[Boolean]) => VariableElimination(e1, e2))
    }

    "produce the correct probability under importance sampling" taggedAs (ExampleTest) in {
      test((e1: Element[Boolean], e2: Element[Boolean]) => Importance(20000, e1, e2))
    }

    "produce the correct probability under Metropolis-Hastings" taggedAs (ExampleTest) in {
      test((e1: Element[Boolean], e2: Element[Boolean]) =>
        MetropolisHastings(8000000, ProposalScheme.default, e1, e2))
    }
  }

  def test(algorithmCreator: (Element[Boolean], Element[Boolean]) => ProbQueryAlgorithm): Unit = {
    Universe.createNew()
    val burglary = Flip(0.01)
    val earthquake = Flip(0.0001)
    val alarm = CPD(burglary, earthquake,
      (false, false) -> Flip(0.001),
      (false, true) -> Flip(0.1),
      (true, false) -> Flip(0.9),
      (true, true) -> Flip(0.99))
    val johnCalls = CPD(alarm,
      false -> Flip(0.01),
      true -> Flip(0.7))
    johnCalls.observe(true)

    val pAlarmGivenBurglary = 0.0001 * 0.99 + 0.9999 * 0.9
    val pAlarmGivenNotBurglary = 0.0001 * 0.1 + 0.9999 * 0.001
    val pJohnGivenBurglary = pAlarmGivenBurglary * 0.7 + (1 - pAlarmGivenBurglary) * 0.01
    val pJohnGivenNotBurglary = pAlarmGivenNotBurglary * 0.7 + (1 - pAlarmGivenNotBurglary) * 0.01
    val qBurglary = 0.01 * pJohnGivenBurglary
    val qNotBurglary = 0.99 * pJohnGivenNotBurglary
    val pBurglary = qBurglary / (qBurglary + qNotBurglary)

    val alg = algorithmCreator(burglary, earthquake)
    alg.start()
    alg.stop()
    alg.probability(burglary, true) should be(pBurglary plusOrMinus 0.01)
    alg.kill()
  }
}
