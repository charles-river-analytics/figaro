/*
 * PriorityMapTest.scala   
 * Priority map tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.util

import org.scalatest.Matchers
import org.scalatest.WordSpec
import math.log
import com.cra.figaro.util._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Performance

class PriorityMapTest extends WordSpec with Matchers {
  "A HeapPriorityMap" should {
    "contain an item after insertion" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      assert(hpm contains "foo")
    }

    "not contain an item after deletion" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm -= "foo"
      assert(!(hpm contains "foo"))
    }

    "map an item to its most recently added score" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm += "foo" -> 5
      hpm("foo") should equal(5)
    }

    "contain the right elements after adding and deleting many elements (forcing resizings) " in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm += "goo" -> 2
      hpm += "hoo" -> 2
      hpm += "ioo" -> 2
      hpm += "joo" -> 2
      hpm += "koo" -> 2
      hpm += "loo" -> 2
      hpm += "moo" -> 2
      hpm += "noo" -> 2
      hpm += "ooo" -> 2
      hpm += "poo" -> 2
      hpm -= "foo"
      hpm -= "hoo"
      hpm -= "joo"
      hpm -= "loo"
      hpm -= "noo"
      hpm -= "poo"
      assert(!(hpm contains "foo"))
      assert(!(hpm contains "hoo"))
      assert(!(hpm contains "joo"))
      assert(!(hpm contains "loo"))
      assert(!(hpm contains "noo"))
      assert(!(hpm contains "poo"))
      assert(hpm contains "goo")
      assert(hpm contains "ioo")
      assert(hpm contains "koo")
      assert(hpm contains "moo")
      assert(hpm contains "ooo")
    }

    "return the minimum element on extraction" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm += "bar" -> 1
      hpm += "baz" -> 3
      hpm.extractMin() should equal(("bar", 1))
      hpm.extractMin() should equal(("foo", 2))
      hpm.extractMin() should equal(("baz", 3))
      an [IllegalArgumentException] should be thrownBy { hpm.extractMin() } 
    }

    "iterate over the elements from lowest to highest" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm += "bar" -> 1
      hpm += "baz" -> 3
      hpm.iterator.toList should equal(List(("bar", 1), ("foo", 2), ("baz", 3)))
    }

    "not modify itself when producing the iterator" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm += "bar" -> 1
      hpm += "baz" -> 3
      hpm.iterator
      hpm.extractMin() should equal(("bar", 1))
      hpm.extractMin() should equal(("foo", 2))
      hpm.extractMin() should equal(("baz", 3))
    }

    "be exactly the same as itself after cloning" in {
      val hpm = new HeapPriorityMap[String, Int]
      hpm += "foo" -> 2
      hpm += "bar" -> 1
      hpm += "baz" -> 3
      val hpm2 = hpm.clone
      hpm2.extractMin() should equal(("bar", 1))
      hpm2.extractMin() should equal(("foo", 2))
      hpm2.extractMin() should equal(("baz", 3))
      an [IllegalArgumentException] should be thrownBy { hpm2.extractMin() } 
    }

    "take roughly log n time for inserting" taggedAs (Performance) in {
      val small = 256
      val large = 512
      def insert(n: Int)() = {
        val h = new HeapPriorityMap[Int, Double]
        for { j <- 1 to n } h += j -> random.nextDouble()
      }
      val time1 = measureTime(insert(small), 20, 100)
      val time2 = measureTime(insert(large), 20, 100)
      // allow slack; if timing is not roughly log n this mark will probably be exceeded
      val slack = 1.1
      time2 / time1 should be < (large.toDouble / small * log(large) / log(small) * slack)
    }

    "take roughly log n time for extracting the minimum element" taggedAs (Performance) in {
      val small = 256
      val large = 512
      def extract(pm: HeapPriorityMap[Int, Double])() = {
        val pm2 = pm.clone
        while (pm2.nonEmpty) pm2.extractMin()
      }
      val pm1 = new HeapPriorityMap[Int, Double]()
      for { j <- 1 to small } { pm1 += j -> random.nextDouble() }
      val pm2 = new HeapPriorityMap[Int, Double]()
      for { j <- 1 to large } { pm2 += j -> random.nextDouble() }
      val time1 = measureTime(extract(pm1), 20, 100)
      val time2 = measureTime(extract(pm2), 20, 100)
      // allow slack; if timing is not roughly log n this mark will probably be exceeded
      val slack = 1.1
      time2 / time1 should be < (large.toDouble / small * log(large) / log(small) * slack)
    }
  }
}
