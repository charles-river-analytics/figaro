/*
 * BooleanResult.scala 
 * Boolean Test
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

class BooleanResult(val name: String, val target: Boolean, val alpha: Double = .90) extends NDTestResult {
  val statistics = BooleanStatistics(target)

  def update(value: Any) {
    value match {
      case x: Boolean => statistics.addValue(x)
      case _ => println(value + " improper value for t-test")
    }
  }

  def check: Boolean = {
    // observed hit rate should be greater than alpha

    val result = statistics.hitRate >= alpha

    result
  }

  def errorMessage = {
    val rate = statistics.hitRate
    f"$name failed with percentage $rate%.2f ($name)"
  }

  case class BooleanStatistics(val target: Boolean) {
    var hits = 0
    var count = 0

    def addValue(value: Boolean) {
      count += 1
      if (target == value) hits += 1
    }

    def hitRate: Double = {
      hits.toDouble / count.toDouble
    }

  }
}