/*
 * CarAndEngineTest.scala 
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
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.language._
import com.cra.figaro.test._

class CarAndEngineTest extends WordSpec with ShouldMatchers {
  "A simple CarAndEngineTest" should {
    "produce the correct probability under variable elimination" taggedAs (ExampleTest) in {
      test((e1: Element[Double]) => VariableElimination(e1))
    }

    "produce the correct probability under importance sampling" taggedAs (ExampleTest) in {
      test((e1: Element[Double]) => Importance(10000, e1))
    }

    "produce the correct probability under Metropolis-Hastings" taggedAs (ExampleTest) in {
      test((e1: Element[Double]) =>
        MetropolisHastings(100000, ProposalScheme.default, e1))
    }
  }

  abstract class Engine extends ElementCollection {
    val power: Element[Symbol]
  }

  private class V8 extends Engine {
    val power: Element[Symbol] = Select(0.8 -> 'high, 0.2 -> 'medium)("power", this)
  }

  private class V6 extends Engine {
    val power: Element[Symbol] = Select(0.2 -> 'high, 0.5 -> 'medium, 0.3 -> 'low)("power", this)
  }

  private object MySuperEngine extends V8 {
    override val power: Element[Symbol] = Constant('high)("power", this)
  }

  class Car extends ElementCollection {
    val engine = Uniform[Engine](new V8, new V6, MySuperEngine)("engine", this)

    val speed = CPD(
      get[Symbol]("engine.power"),
      'high -> Constant(90.0),
      'medium -> Constant(80.0),
      'low -> Constant(70.0))
  }

  def main(args: Array[String]) {

  }

  def test(algorithmCreator: (Element[Double]) => ProbQueryAlgorithm): Unit = {
    Universe.createNew()
    val car = new Car

    val alg = algorithmCreator(car.speed)
    alg.start()
    alg.stop()
    alg.expectation(car.speed, (d: Double) => d) should be(85.6 plusOrMinus 0.5)
    alg.kill()
  }
}
