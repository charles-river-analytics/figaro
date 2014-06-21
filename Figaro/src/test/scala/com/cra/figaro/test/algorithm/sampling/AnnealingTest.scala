/*
 * AnnealingTest.scala  
 * Annealing tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.sampling

import org.scalatest._
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.compound.^^
import com.cra.figaro.test._

class AnnealingTest extends WordSpec with Matchers with PrivateMethodTester {

  def build1(n: Int, prev: List[Element[Double]]): List[Element[Double]] = if (n == 0) return prev else build1(n - 1, prev :+ Normal(prev.last, 1.0))

  def buildUndirected(n: Int, prev: Element[Boolean], fcn: ((Boolean, Boolean)) => Double): Unit = if (n == 0) return else {
    val f = Flip(.5)
    val a = ^^(f, prev)("a" + n, Universe.universe)
    a.addConstraint(fcn)
    buildUndirected(n - 1, f, fcn)
  }

  "Running Annealing" should {
    "give the most likely value for every active element" in {
      Universe.createNew()
      val elems = build1(4, List(Constant(0)))
      val annealer = MetropolisHastingsAnnealer(ProposalScheme.default, Schedule.default(2.0))
      annealer.start()
      Thread.sleep(500)
      annealer.stop()
      elems.foreach { e => annealer.mostLikelyValue(e) should be > Double.MinValue }
      annealer.kill
    }

    "converge to the most likely value" in {
      Universe.createNew()
      val elems = buildUndirected(4, Flip(0.5), (b: (Boolean, Boolean)) => if (b._1 && b._2) 2.0 else 1.0)
      val annealer = MetropolisHastingsAnnealer(ProposalScheme.default, Schedule.default(2.0))
      annealer.start()
      Thread.sleep(500)
      annealer.stop()
      for { i <- 1 to 4 } {
        val a = Universe.universe.getElementByReference[(Boolean, Boolean)]("a" + i)
        println(i)
        annealer.mostLikelyValue(a) should equal(true, true)
      }
      annealer.kill
    }

    "produce higher temperatue with higher k" in {
      Universe.createNew()
      val elems1 = buildUndirected(4, Flip(0.5), (b: (Boolean, Boolean)) => if (b._1 && b._2) 2.0 else 1.0)
      val annealer1 = MetropolisHastingsAnnealer(1000, ProposalScheme.default, Schedule.default(4.0))
      annealer1.start()
      val temp1 = annealer1.getTemperature

      val elems2 = buildUndirected(4, Flip(0.5), (b: (Boolean, Boolean)) => if (b._1 && b._2) 2.0 else 1.0)
      val annealer2 = MetropolisHastingsAnnealer(1000, ProposalScheme.default, Schedule.default(2.0))
      annealer2.start()
      val temp2 = annealer2.getTemperature

      temp2 should be > temp1
      
      annealer1.kill
      annealer2.kill
    }

    "increase the temperature with more iterations" in {
      Universe.createNew()
      val elems1 = buildUndirected(4, Flip(0.5), (b: (Boolean, Boolean)) => if (b._1 && b._2) 2.0 else 1.0)
      val annealer1 = MetropolisHastingsAnnealer(ProposalScheme.default, Schedule.default(2.0))
      annealer1.start()
      Thread.sleep(250)
      annealer1.stop
      val temp1 = annealer1.getTemperature

      annealer1.resume()
      Thread.sleep(250)
      annealer1.stop

      val temp2 = annealer1.getTemperature

      temp2 should be > temp1
      annealer1.kill
    }

  }

}
