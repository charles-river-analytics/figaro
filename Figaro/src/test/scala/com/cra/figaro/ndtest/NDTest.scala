/*
 * NDTest.scala 
 * Runs non-deterministic tests
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Glenn Takata (gtakata@cra.com), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Mar 17, 2015
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.ndtest

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.collection.mutable.Map

abstract class NDTest extends WordSpec with Matchers {
  val results: Map[String, NDTestResult] = Map()

  def update(value: Any, test: String, name: String, target: Any, alpha: Double = .05) =
  {
    if (!results.contains(name))
    {
      results.put(name, createResult(test, name, target, alpha))
    }
    results(name).update(value)
  }
  
  def createResult(test: String, name: String, target: Any, alpha: Double): NDTestResult = {
    test match {
      case NDTest.TTEST => new TTestResult(name, target.asInstanceOf[Double], alpha)
      case NDTest.BOOLEAN => new BooleanResult(name, target.asInstanceOf[Boolean], alpha)
      case _ => new NoTestResult(name)
    }  
  }
  
  final def run(n: Int, clear: Boolean = true)
  {
    if (clear) results.clear()
    
    (0 until n).foreach(_ => oneTest)
    for (result <- results.values) {
      assert(result.check, result.errorMessage)
    }
  }

  def oneTest: Unit
}

object NDTest {
  val TTEST = "TTest-type"
  val BOOLEAN = "BooleanTest-type"
}
