/*
 * TTestResult.scala 
 * T Test
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Glenn Takata (gtakata@cra.com), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Mar 19, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.ndtest

import scala.collection.mutable.ListBuffer
import org.apache.commons.math3.stat.descriptive.SummaryStatistics

class TTestResult(name: String, target: Double, alpha: Double = .05) extends NDTestResult[Double] {
  val statistics = new SummaryStatistics()

  def update(value: Double) {
    statistics.addValue(value)
  }

  def check: Boolean = {
    val tester = new org.apache.commons.math3.stat.inference.TTest
    // Apache Commons Math T Test
    // Returns false if the test passed and true if the test fails, so reverse this for return value      
    !tester.tTest(target, statistics, alpha)
  }
}