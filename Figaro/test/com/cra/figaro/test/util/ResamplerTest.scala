/*
 * ResamplerTest.scala   
 * Resampler tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.test.util

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import com.cra.figaro.util._
import java.util.{ Map, TreeMap }

class ResamplerTest extends WordSpec with PrivateMethodTester with ShouldMatchers {
  "A MapResampler's map" should {
    "contain as values all the samples and none other" in {
      val r = new MapResampler(List(0.5 -> 1, 0.25 -> 2, 0.5 -> 3))
      val v = getMap(r).values
      assert(v contains 1)
      assert(v contains 2)
      assert(v contains 3)
      assert(v.size == 3)
    }

    "have the difference between a key and the next be the normalized weight of the sample" in {
      val r = new MapResampler(List(0.5 -> 1, 0.25 -> 2, 0.5 -> 3))
      val es = getMap(r).entrySet.iterator
      var key1, key2, key3: Double = -1.0
      while (es.hasNext()) {
        val e = es.next()
        val k = e.getKey
        val v = e.getValue
        if (v == 1) key1 = k
        else if (v == 2) key2 = k
        else if (v == 3) key3 = k
      }
      key1 should be(0.0 plusOrMinus 0.0000000001)
      key2 should be(0.4 plusOrMinus 0.0000000001)
      key3 should be(0.6 plusOrMinus 0.0000000001)
    }
  }

  "A Resampler" should {
    "produce an element with probability equal to the normalized weight of the element" in {
      val numSamples = 50000
      val r = new MapResampler(List(0.5 -> 1, 0.25 -> 2, 0.5 -> 3))
      val totals = Array.fill(3)(0)
      for { i <- 1 to numSamples } {
        totals(r.resample() - 1) += 1
      }
      totals(0).toDouble / numSamples should be(0.4 plusOrMinus 0.01)
      totals(1).toDouble / numSamples should be(0.2 plusOrMinus 0.01)
      totals(2).toDouble / numSamples should be(0.4 plusOrMinus 0.01)
    }
  }

  def getMap[T](r: Resampler[T]) = {
    val getTheMap = PrivateMethod[TreeMap[Double, T]]('getTheMap)
    r invokePrivate getTheMap()
  }
}