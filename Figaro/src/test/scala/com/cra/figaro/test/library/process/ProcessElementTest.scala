/*
 * ProcessElementTest.scala
 * Process element tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Nov 27, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.library.process

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.library.process._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound._

class ProcessElementTest extends WordSpec with Matchers {
  "A process element" should {
    "get the right element using apply" in {
      val procElem = create()
      VariableElimination.probability(procElem(0), true) should be ((0.5 * 0.1 + 0.5 * 0.3) +- 0.0000000001)
    }

    "get the right optional element using get" in {
      val procElem = create()
      VariableElimination.probability(procElem.get(2), Some(true)) should be ((0.5 * 0.5) +- 0.000000000001)
    }

    "map a function through all possible values correctly" in {
      val procElem = create()
      VariableElimination.probability(procElem.map(!_)(0), false) should be ((0.5 * 0.1 + 0.5 * 0.3) +- 0.0000000001)
    }

    "chain a function through all possible values correctly" in {
      val procElem = create()
      val p1 = 0.5 * 0.1 + 0.5 * 0.3
      val p2 = 1 - p1
      val answer = p1 * 0.6 + p2 * 0.9
      VariableElimination.probability(procElem.chain(if (_) Flip(0.6) else Flip(0.9))(0), true) should be (answer +- 0.0000001)
    }
  }

  def create() = {
    Universe.createNew()
    val elem1 = Flip(0.1)
    val elem2 = Flip(0.2)
    val elem3 = Flip(0.3)
    val elem4 = Flip(0.4)
    val elem5 = Flip(0.5)
    val process1: Process[Int, Boolean] = Container(elem1, elem2)
    val process2: Process[Int, Boolean] = Container(elem3, elem4, elem5)
    val processChooser = Select(0.5 -> process1, 0.5 -> process2)
    new ProcessElement(processChooser)
  }
}
