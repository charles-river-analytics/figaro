/*
 * Test.scala  
 * Test runner for com.cra.figaro.test.example.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.example

private object Test {
  def main(args: Array[String]) = {
    (new OpenUniverseTest).execute()
    (new BayesianNetworkTest).execute()
    (new SimpleMovieTest).execute()
    (new MutableMovieTest).execute()
    (new SmokersTest).execute()
    (new FirmsTest).execute()
    (new MultiValuedTest).execute()
    (new SingleDecisionTest).execute()
    (new MultiDecisionTest).execute()

  }
}
