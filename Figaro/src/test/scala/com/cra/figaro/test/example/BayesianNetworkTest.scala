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

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.factored._
import beliefpropagation.BeliefPropagation
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example
import com.cra.figaro.test.tags.NonDeterministic

class BayesianNetworkTest extends WordSpec with Matchers {
  "A simple Bayesian network" should {
    "produce the correct probability under variable elimination" taggedAs (Example, NonDeterministic) in {
      val model = new Model
      VariableElimination.probability(model.burglary, true) should be (model.pBurglary +- 0.01)
    }

    "produce the correct probability under belief propagation" taggedAs (Example, NonDeterministic) in {
      val model = new Model
      BeliefPropagation.probability(model.burglary, true) should be (model.pBurglary +- 0.01)
    }

    "produce the correct probability under importance sampling" taggedAs (Example, NonDeterministic) in {
      val model = new Model
      Importance.probability(model.burglary, true) should be (model.pBurglary +- 0.02)
    }
    
    "produce the correct probability under Metropolis-Hastings without burn-in or interval" taggedAs (Example, NonDeterministic) in {
      val model = new Model
      MetropolisHastings.probability(model.burglary, true) should be (model.pBurglary +- 0.02)
    }
    
    "produce the correct probability under Metropolis-Hastings with burn-in and interval" taggedAs (Example, NonDeterministic) in {
      val model = new Model
      val alg = MetropolisHastings(200000, ProposalScheme.default, 800, 10, model.burglary)
      alg.start()
      alg.probability(model.burglary, true) should be (model.pBurglary +- 0.02)
    }
  }

  class Model {
    Universe.createNew()
    val burglaryProb = 0.1
    val earthquakeProb = 0.001
    val alarmGivenNotBNotEProb = 0.003
    val alarmGivenNotBEProb = 0.1
    val alarmGivenBNotEProb = 0.9
    val alarmGivenBEProb = 0.99
    val johnGivenNotAProb = 0.01
    val johnGivenAProb = 0.7
    val burglary = Flip(burglaryProb)
    val earthquake = Flip(earthquakeProb)
    val alarm = CPD(burglary, earthquake,
      (false, false) -> Flip(alarmGivenNotBNotEProb),
      (false, true) -> Flip(alarmGivenNotBEProb),
      (true, false) -> Flip(alarmGivenBNotEProb),
      (true, true) -> Flip(alarmGivenBEProb))
    val johnCalls = CPD(alarm,
      false -> Flip(johnGivenNotAProb),
      true -> Flip(johnGivenAProb))
    johnCalls.observe(true)

    val pAlarmGivenBurglary = earthquakeProb * alarmGivenBEProb + (1 - earthquakeProb) * alarmGivenBNotEProb
    val pAlarmGivenNotBurglary = earthquakeProb * alarmGivenNotBEProb + (1 - earthquakeProb) * alarmGivenNotBNotEProb
    val pJohnGivenBurglary = pAlarmGivenBurglary * johnGivenAProb + (1 - pAlarmGivenBurglary) * johnGivenNotAProb
    val pJohnGivenNotBurglary = pAlarmGivenNotBurglary * johnGivenAProb + (1 - pAlarmGivenNotBurglary) * johnGivenNotAProb
    val qBurglary = burglaryProb * pJohnGivenBurglary
    val qNotBurglary = (1 - burglaryProb) * pJohnGivenNotBurglary
    val pBurglary = qBurglary / (qBurglary + qNotBurglary)
  }
}
