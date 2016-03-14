/*
 * FactoredAlgorithmTest.scala
 * Variable elimination tests.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.util._
import com.cra.figaro.test._
import scala.collection.mutable.Map
import com.cra.figaro.test.tags.Performance
import com.cra.figaro.test.tags.NonDeterministic

class FactoredAlgorithmTest extends WordSpec with Matchers {
  "A Factored Algorithm" when {
    "getting needed elements" should {
      "return factors originating from the targets and evidence" in {
        Universe.createNew()

        val teacherSkill = Flip(0.6)
        val studentAbility = Flip(0.7)
        val pass = Chain(studentAbility, (ability: Boolean) =>
          if (ability) If(teacherSkill, Flip(0.9), Flip(0.7))
          else If(teacherSkill, Flip(0.6), Flip(0.3)))
        pass.observe(false)
        val ve = VariableElimination(teacherSkill)
        val (neededElements, _) = ve.getNeededElements(List(teacherSkill), Int.MaxValue )
        neededElements.size should equal(9)        
      }
    }
  }

   
}
