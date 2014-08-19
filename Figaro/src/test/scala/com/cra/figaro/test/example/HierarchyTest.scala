/*
 * HiearchyTest.scala 
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
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example
import com.cra.figaro.test.tags.NonDeterministic

class HierarchyTest extends WordSpec with Matchers {

  var universe = Universe.createNew

  "A simple HierarchyTest" should {
    "produce the correct probability under variable elimination" taggedAs (Example, NonDeterministic) in {
      test((e1: Element[Boolean], e2: Element[String]) => VariableElimination(e1, e2))
    }

    "produce the correct probability under importance sampling" taggedAs (Example, NonDeterministic) in {
      test((e1: Element[Boolean], e2: Element[String]) => Importance(10000, e1, e2))
    }

    "produce the correct probability under Metropolis-Hastings" taggedAs (Example, NonDeterministic) in {
      test((e1: Element[Boolean], e2: Element[String]) =>
        MetropolisHastings(200000, ProposalScheme.default, e1, e2))
    }
  }

  def shortClassName[T](elem: Element[T]): Element[String] =
    Apply(elem, (t: T) => {
      val fullName = t.getClass().getName()
      val dollar = fullName.indexOf('$')
      fullName.substring(dollar + 1, fullName.length)
    })

  abstract class Vehicle extends ElementCollection {
    val size: Element[Symbol]
    val speed: Element[Int]
    lazy val capacity: Element[Int] = Constant(0)
  }

  class Truck extends Vehicle {
    val size: Element[Symbol] = Select(0.25 -> 'medium, 0.75 -> 'big)("size", this)
    val speed: Element[Int] = Uniform(50, 60, 70)("speed", this)
    override lazy val capacity: Element[Int] = Chain(size, (s: Symbol) => if (s == 'big) Select(0.5 -> 1000, 0.5 -> 2000); else Constant(100))("capacity", this)
  }

  class Pickup extends Truck {
    override val speed: Element[Int] = Uniform(70, 80)("speed", this)
    override val size: Element[Symbol] = Constant('medium)("size", this)
  }

  class TwentyWheeler extends Truck {
    override val size: Element[Symbol] = Constant('huge)("size", this)
    override lazy val capacity = Constant(5000)("capacity", this)
  }

  class Car extends Vehicle {
    val size = Constant('small)("size", this)
    val speed = Uniform(70, 80)("speed", this)
  }

  object Vehicle {
    def generate(name: String): Element[Vehicle] =
      Dist(0.6 -> Car.generate, 0.4 -> Truck.generate)(name, universe)
  }

  object Truck {
    def generate: Element[Vehicle] = Dist(0.1 -> TwentyWheeler.generate, 0.3 -> Pickup.generate, 0.6 -> Constant[Vehicle](new Truck))
  }

  object Pickup {
    def generate: Element[Vehicle] = Constant(new Pickup)
  }

  object TwentyWheeler {
    def generate: Element[Vehicle] = Constant(new TwentyWheeler)
  }

  object Car {
    def generate: Element[Vehicle] = Constant(new Car)
  }

  def test(algorithmCreator: (Element[Boolean], Element[String]) => ProbQueryAlgorithm): Unit = {
    universe = Universe.createNew
    val myVehicle = Vehicle.generate("v1")
    val name = shortClassName(myVehicle)
    universe.assertEvidence(List(NamedEvidence("v1.size", Observation('medium))))
    val isPickup = Apply(myVehicle, (v: Vehicle) => v.isInstanceOf[Pickup])
    val alg = algorithmCreator(isPickup, name)
    alg.start()
    alg.stop()
    alg.probability(isPickup, true) should be(.666 +- 0.02)
    alg.kill
  }
}
