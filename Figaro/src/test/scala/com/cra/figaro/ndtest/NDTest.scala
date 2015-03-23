/*
 * NDTest.scala 
 * Runs non-deterministic tests
 * 
 * Created By:      Michael Reposa (mreposa@cra.com) and Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 17, 2015
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.ndtest

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException
import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import scala.collection.mutable.Map

abstract class NDTest extends WordSpec with Matchers {
  val results: Map[String, NDTestResult[_]] = Map()

  def update[T](name: String, value: T) =  results(name).asInstanceOf[NDTestResult[T]].update(value)
  
  final def run(n: Int) {
    (0 until n).foreach(_ => oneTest)
    results.values.foreach(_.check should be(true))
  }

  def oneTest: Unit
}
