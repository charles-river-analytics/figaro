/*
 * Firms.scala
 * An example with rich constraints.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.compound._

/**
 * An example with rich constraints.
 */
object Firms {
  private class Firm {
    val efficient = Flip(0.3)
    val bid = If(efficient, continuous.Uniform(5, 15), continuous.Uniform(10, 20))
  }

  private val firms = Array.fill(20)(new Firm)
  private val winner = discrete.Uniform(firms: _*)
  private val winningBid = Chain(winner, (f: Firm) => f.bid)
  winningBid.setConstraint((d: Double) => 20 - d)

  def main(args: Array[String]) {
    val winningEfficiency = Chain(winner, (f: Firm) => f.efficient)
    val alg = Importance(winningEfficiency)
    alg.start()
    Thread.sleep(1000)
    alg.stop()
    println("Probability the winner is efficient: " + alg.probability(winningEfficiency, true))
    alg.kill()
  }
}