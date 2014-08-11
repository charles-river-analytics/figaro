/*
 * FirmsTest.scala 
 * Firms examples tests. 
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
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Example

class FirmsTest extends WordSpec with Matchers {
  "The firms example" should {
    "produce the correct answer under importance sampling" taggedAs (Example) in {
      test((e: Element[Boolean]) => Importance(20000, e))
    }

    "produce the correct answer under Metropolis-Hastings" taggedAs (Example) in {
      test((e: Element[Boolean]) => MetropolisHastings(3000000, ProposalScheme.default, 30000, e))
    }

    "produce the correct answer under variable elimination" taggedAs (Example) in {
      test((e: Element[Boolean]) => VariableElimination(e))
    }
  }

  private class Firm(name: String) {
    val efficient = Flip(0.3)(name + "efficient", universe)
    val bid = If(
      efficient,
      continuous.Uniform(5.0, 15.0)(name + "bidWhenEfficient", universe),
      continuous.Uniform(10.0, 20.0)(name + "bidWhenInefficient", universe))(name + "bid", universe)
  }

  def test(algorithmCreator: Element[Boolean] => ProbQueryAlgorithm) {
    Universe.createNew()
    val firm1 = new Firm("Firm1")
    val firm2 = new Firm("Firm2")
    val firms = Array(firm1, firm2)
    val winner = discrete.Uniform(firms: _*)("winner", universe)
    val winningBid = CachingChain(winner, (f: Firm) => f.bid)("winningBid", universe)
    winningBid.setConstraint((d: Double) => 20 - d)
    val winningEfficiency = CachingChain(winner, (f: Firm) => f.efficient)("winningEfficiency", universe)

    // Expected constraint for efficient firm = 0.1 \int_5^15 (20 - x) dx
    // = 0.1 [20x - 0.5 x^2]_5^15 = 0.1 (300 - 112.5 - 100 + 12.5) = 10
    // Expected constraint for inefficient firm = 0.1 \int_10^20 (20 - x) dx
    // = 0.1 [20x - 0.5 x^2]_10^20 = 0.1 (400 - 200 - 200 + 50) = 5
    // Code: pyz = probability 0's efficiency is y, 1's efficiency is z
    // Assume wlog 0 is the winner
    val pff = 0.7 * 0.7 * 5
    val pft = 0.7 * 0.3 * 5
    val ptf = 0.3 * 0.7 * 10
    val ptt = 0.3 * 0.3 * 10
    val answer = (ptf + ptt) / (pff + pft + ptf + ptt)
    val alg = MetropolisHastings(200000, ProposalScheme.default, winningEfficiency)
    val bid1WhenEfficient: Element[Double] = universe.get("Firm1bidWhenEfficient")
    alg.start()
    alg.probability(winningEfficiency, true) should be(answer +- 0.01)
    alg.kill
  }

}
