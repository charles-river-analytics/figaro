/*
 * ContinuousExample.scala 
 * An example using continuous elements. 
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jul 3, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.example.visualization

import com.cra.figaro.library.atomic.continuous.{Exponential, Normal}
import com.cra.figaro.util.visualization._
import com.cra.figaro.util.visualization.results.{ContinuousData}

/**
 * @author gtakata
 */
object ContinuousExample {

  def main(args: Array[String]) { 
    val gui = ResultsGUI
    gui.startup(args)
    gui.addResult("normal", Normal(20, 25))
    gui.addResult("exponential", Exponential(.2))
  }
}