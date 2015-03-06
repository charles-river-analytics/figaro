/*
 * Importance.scala
 * Importance sampler.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.ndtest

/**
 * @author Glenn Takata Mar 5, 2015
 *
 */
trait Result {
  def update(value: Double)
  
  def report
}

class NDTest {
  var results: Map[String, Result] = Map()
  
  def run(n: Int) {
    (0 to n).foreach(_ => test)
    
    for (result <- results.values)
      result.report
  }
  
  def test {
//    val result1 = results.get("probResult")
//    result1.update(newValue)
  }
  
  class MeanResult(name: String, target: Double) extends Result {
    var statistic: Double = 0
    
    def update(value: Double) {
      statistic += value
    }
    
    def report {}
  }
}