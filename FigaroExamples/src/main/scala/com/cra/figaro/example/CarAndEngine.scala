/*
 * CarAndEngine.scala
 * A probabilistic relational model example with reference uncertainty.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.atomic.continuous.Normal

/**
 * A probabilistic relational model example with reference uncertainty.
 */
object CarAndEngine {
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
    val car = new Car
    val alg = VariableElimination(car.speed)
    alg.start()
    alg.stop()
    println(alg.expectation(car.speed)(d => d))
    alg.kill()
  }
}
