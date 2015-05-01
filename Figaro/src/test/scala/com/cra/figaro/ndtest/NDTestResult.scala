/*
 * NDTestResult.scala 
 * Accumulates the results of a non-deterministic test run
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Glenn Takata (gtakata@cra.com), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Mar 17, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.ndtest

trait NDTestResult  {
  def name : String
  def update(value: Any)
  def check: Boolean
  def errorMessage: String
}