/*
 * MultiValuedReferenceUncertainty.scala
 * A simple model example with multi-valued reference uncertainty and aggregates.
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

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.util.MultiSet

/**
 * A simple model example with multi-valued reference uncertainty and aggregates.
 */
object MultiValuedReferenceUncertainty {
  class Component extends ElementCollection {
    val f = Select(0.2 -> 2, 0.3 -> 3, 0.5 -> 5)("f", this)
  }

  val specialComponent1 = new Component
  val specialComponent2 = new Component

  def makeComponent = () => Select(0.1 -> specialComponent1, 0.2 -> specialComponent2, 0.7 -> new Component)

  class Container extends ElementCollection {
    val components = MakeList(Select(0.5 -> 1, 0.5 -> 2), makeComponent)("components", this)

    val sum = getAggregate((xs: MultiSet[Int]) => (0 /: xs)(_ + _))("components.f")
  }

  def main(args: Array[String]): Unit = {
    val c = new Container
    val alg = Importance(100000, c.sum)
    alg.start()
    println(alg.distribution(c.sum).toList)
    alg.kill
  }
}
