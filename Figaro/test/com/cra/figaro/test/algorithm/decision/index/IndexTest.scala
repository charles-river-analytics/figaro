/*
 * IndexTest.scala  
 * Variable elimination tests.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.test.algorithm.decision.index

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.decision._
import com.cra.figaro.algorithm.decision.index._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.util._
import com.cra.figaro.test._
import scala.collection.immutable.Map

class IndexTest extends WordSpec with ShouldMatchers {

  "An Index" when {

    "Nearest Neighbors retrieved" should {
      "return the correct neighbors with a VPIndex" in {
        Universe.createNew()
        val (d, strat_map) = DecisionContinuous(false)
        val index1 = FlatIndex[d.PValue, d.DValue](strat_map)
        val index2 = VPIndex[d.PValue, d.DValue](strat_map, 50)

        val nn1 = index1.getNN(0.0, 100)
        val nn2 = index2.getNN(0.0, 100)
        val diff = nn1.diff(nn2)
        diff should be('empty)
      }
    }

  }

  def DecisionContinuous(sim: Boolean): (Decision[Double, Boolean], Map[(Double, Boolean), DecisionSample]) = {
    // Closely replicates the discrete test. Should be pretty close to the same decisions, 
    // but some tests may fail (inconsistently)
    Universe.createNew()
    val p = Select[Int](0.25 -> 0, 0.75 -> 2)
    val pp = Uniform[Int](-2, 0)
    val ppp = Chain(p, pp, (a: Int, b: Int) => Normal((a + b).toDouble, 0.1))

    val d = NonCachingDecision[Double, Boolean](ppp, List(true, false))
    def u_fcn(b: Boolean, i: Int): Double = {
      i match {
        case 0 => if (b) -7.0 else 6.0
        case 2 => if (b) 1.0 else -2.0
      }
    }
    val u = Apply[Boolean, Int, Double](d, p, u_fcn)

    val ve = DecisionImportance(10000, List(u), d)
    ve.start()
    (d, ve.getUtility().asInstanceOf[Map[(Double, Boolean), DecisionSample]])
  }

}
  
  
  
  