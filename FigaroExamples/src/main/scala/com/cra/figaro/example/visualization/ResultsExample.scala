package com.cra.figaro.example.visualization

import com.cra.figaro.util.visualization._
import com.cra.figaro.util.visualization.results.ResultsData

/**
 * @author gtakata
 */
object ResultsExample {
  val result1 = ResultsData("test", List((.20, true), (.80, false)))
  val result2 = ResultsData("test2", List((1.0, true), (.00, false)))

  def main(args: Array[String]) { 
    val gui = ResultsGUI
    gui.startup(args)
    gui.addResult(result1)
    gui.addResult(result2)
  }
}