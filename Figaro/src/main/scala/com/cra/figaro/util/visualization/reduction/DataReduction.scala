/*
 * DataReduction.scala 
 * Setup and display distributions based on continuous element data
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jul 6, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization.reduction

import scala.collection._

object DataReduction {
  def binToDistribution(data: List[(Double, Double)]): List[(Double, Double)] = {
    if (data.size > 50) {
      var mean = 0.0
      var totalProb = 0.0
      var count = 0
      var min = Double.MaxValue
      var max = Double.MinValue
      var ss = 0.0

      for ((prob, value) <- data) {
        totalProb += prob
        mean += prob * value
        min = math.min(min, value)
        max = math.max(max, value)
        ss += prob * value * value
        count += 1
      }

      val variance = ss - mean * mean
      val sd = math.sqrt(variance)

      val distMax = mean + 3 * sd
      val distMin = mean - 3 * sd
      
      val nInterval = math.min(count, 300)
      val interval = (distMax - distMin) / nInterval
      var dist = Array.fill[Double](nInterval)(0)

      for ((prob, value) <- data) {
        val pos = math.max(math.min(math.floor((value - distMin) / interval).toInt, nInterval - 1), 0)
        val posProb = dist(pos)
        dist(pos) = posProb + prob
      }

      val probDist = for (i <- 1 to nInterval) yield {
        val value = distMin + i * interval
        (dist(i - 1), value)
      }

      probDist.toList
    } else {
      data
    }
  }
}