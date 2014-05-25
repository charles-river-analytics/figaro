/*
 * LazyAlgorithm.scala
 * Lazy algorithms.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 28, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

trait LazyAlgorithm extends Algorithm {
  var depth = 0
  
  def run(depth: Int): Unit
  
  def doStart() { pump() }
  
  def pump() { depth += 1; run(depth) }
  
  def doStop() { }
  
  def doResume() { pump() }
  
  def doKill() { depth = -1 }
}
