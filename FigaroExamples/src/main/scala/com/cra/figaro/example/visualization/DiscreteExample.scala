/*
 * DiscreteExample.scala 
 * An example using discrete distributions that demonstrates user interaction. 
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jun 16, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.example.visualization

import com.cra.figaro.util.visualization._
import com.cra.figaro.util.visualization.results.{DiscreteData}

/**
 * @author gtakata
 */
object DiscreteExample {

  def main(args: Array[String]) { 
    val gui = ResultsGUI
    gui.startup(args)
    gui.addResult("test", List((.20, true), (.80, false)))
    gui.addResult("test2", List((1.0, true), (.00, false)))

  }
}