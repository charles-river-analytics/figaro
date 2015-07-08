/*
 * Regression.scala
 * A Bayesian network example with visualization
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jul 7, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.example.visualization

import com.cra.figaro.language.{Universe}
import com.cra.figaro.algorithm.sampling.{Importance}
import com.cra.figaro.library.atomic.continuous.{Normal, Uniform}
import com.cra.figaro.util.visualization.ResultsGUI

/**
 * @author Glenn Takata (gtakata@cra.com)
 */
object Regression {
    Universe.createNew()

  private val mean = Uniform(0, 1)

  for (_ <- 0 until 100) {
    val n = Normal(mean, 1.0)
    n.addConstraint((m: Double) => m + 5)
  }
  

  def main(args: Array[String]) {
    val gui = ResultsGUI
    gui.startup(args)

    val alg = Importance(1000, mean)
    alg.start()
    
    val dist = alg.distribution(mean)
    println("Probability of mean: " + dist.toList)
    
    gui.addResult("mean", alg.distribution(mean).toList)

    alg.kill
  }
}