/*
 * NoTestResult.scala 
 * No Test
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Glenn Takata (gtakata@cra.com), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Apr 8, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.ndtest

import scala.collection.mutable.ListBuffer
import org.apache.commons.math3.stat.descriptive.SummaryStatistics

class NoTestResult(val name: String) extends NDTestResult {

  def update(value: Any) {
  }

  def check: Boolean = {
    false
  }
  
  def errorMessage = "No test for " + name

}