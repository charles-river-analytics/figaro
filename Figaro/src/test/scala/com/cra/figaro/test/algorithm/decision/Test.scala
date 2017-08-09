/*
 * Test.scala 
 * Test runner for com.cra.figaro.test.algorithm.decision.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.decision

import com.cra.figaro.algorithm.factored._

private object Test {
  def main(args: Array[String]) {
    (new DecisionVETest).execute()
    (new DecisionImportanceTest).execute()
    (new DecisionMetropolisHastingsTest).execute()
  }
}
