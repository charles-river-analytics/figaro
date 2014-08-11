/*
 * SelectableSetTest.scala   
 * Selectable set tests.
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
import org.scalatest.{ WordSpec, PrivateMethodTester }
import math.log
import com.cra.figaro.util._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Performance

class SelectableSetTest extends WordSpec with PrivateMethodTester with Matchers {
  "A SelectableSet" should {   
    "contain an element after insertion" in {
      val s = new HashSelectableSet[Int]
      s.add(1)
      s.contains(1) should be(true)
    }

    "not contain an element after removal" in {
      val s = new HashSelectableSet[Int]
      s.add(1)
      s.remove(1)
      s.contains(1) should be(false)
    }

    "convert to a list containing the elements it contains" in {
      val s = new HashSelectableSet[Int]
      // make sure to resize
      for { i <- 1 to 1000 } { s.add(i) }
      s.remove(2)
      val l = s.toList
      l.size should equal(999)
      for { i <- 1 to 1000 } { if (i != 2) l should contain(i) }
    }

    "return an element uniformly at random when selecting" in {
      val s = new HashSelectableSet[Int]
      s.add(1)
      s.add(2)
      s.add(3)
      s.add(6)
      var successes = 0
      val numTrials = 10000
      for { i <- 1 to numTrials } { if (s.select() == 2) successes += 1 }
      successes.toDouble / numTrials should be(0.25 +- 0.1)
    }

    "create a separate copy of itself when cloning" in {
      val s1 = new HashSelectableSet[Int]
      s1.add(1)
      val s2 = s1.clone
      s1.remove(1)
      s2.contains(1) should be(true)
    }

    "take roughly constant time for insertion" taggedAs (Performance) in {
      val small = 2000
      val large = 4000
      def insert(n: Int)() = {
        val s = new HashSelectableSet[Int]
        for { j <- 1 to n } s.add(j)
      }
      val time1 = measureTime(insert(small), 20, 100)
      val time2 = measureTime(insert(large), 20, 100)
      // allow slack; if timing is not roughly constant this mark will probably be exceeded
      val slack = 1.2
      time2 / time1 should be < (large.toDouble / small * slack)
    }

    "take roughly constant time for removal" taggedAs (Performance) in {
      val small = 2000
      val large = 4000
      def remove(n: Int, s: SelectableSet[Int])() = {
        val copy = s.clone
        for { j <- 1 to n } copy.remove(j)
      }
      val s1 = new HashSelectableSet[Int]
      for { j <- 1 to small } s1.add(j)
      val s2 = new HashSelectableSet[Int]
      for { j <- 1 to large } s2.add(j)
      val time1 = measureTime(remove(small, s1), 20, 100)
      val time2 = measureTime(remove(large, s2), 20, 100)
      // allow slack
      val slack = 1.2
      time2 / time1 should be < (large.toDouble / small * slack)
    }

    "take roughly constant time for searching" taggedAs (Performance) in {
      val small = 2000
      val large = 4000
      def search(n: Int, s: SelectableSet[Int])() =
        for { j <- 1 to 2 * n } s.contains(j) // search for both present and absent elements
      val s1 = new HashSelectableSet[Int]
      for { j <- 1 to small } s1.add(j)
      val s2 = new HashSelectableSet[Int]
      for { j <- 1 to large } s2.add(j)
      val time1 = measureTime(search(small, s1), 20, 100)
      val time2 = measureTime(search(large, s2), 20, 100)
      // allow slack
      val slack = 1.2
      time2 / time1 should be < (large.toDouble / small * slack)
    }

    "take roughly log n time for selection" taggedAs (Performance) in {
      val small = 1024
      val large = 2048
      def select(n: Int, s: SelectableSet[Double])() =
        for { j <- 1 to n } s.select()
      val s1 = new HashSelectableSet[Double]
      for { j <- 1 to small } s1.add(random.nextDouble())
      val s2 = new HashSelectableSet[Double]
      for { j <- 1 to large } s2.add(random.nextDouble())
      val time1 = measureTime(select(small, s1), 20, 100)
      val time2 = measureTime(select(large, s2), 20, 100)
      // allow slack
      val slack = 1.1
      time2 / time1 should be < (large.toDouble / small * log(large) / log(small) * slack)
    }

    "take roughly linear time for enumeration" taggedAs (Performance) in {
      def enumerate(s: SelectableSet[Double])() =
        s.toList
      val size1 = 1000
      val size2 = 2000
      val size3 = 3000
      val size4 = 4000
      val s1 = new HashSelectableSet[Double]
      for { j <- 1 to size1 } s1.add(random.nextDouble())
      val s2 = new HashSelectableSet[Double]
      for { j <- 1 to size2 } s2.add(random.nextDouble())
      val s3 = new HashSelectableSet[Double]
      for { j <- 1 to size3 } s3.add(random.nextDouble())
      val s4 = new HashSelectableSet[Double]
      for { j <- 1 to size4 } s4.add(random.nextDouble())
      val time1 = measureTime(enumerate(s1), 20, 100)
      val time2 = measureTime(enumerate(s2), 20, 100)
      val time3 = measureTime(enumerate(s3), 20, 100)
      val time4 = measureTime(enumerate(s4), 20, 100)
      // For a linear function, the difference should be the same if the differences in sizes is the same;
      // we allow some slack
      val slack = 1.1
      (time4 - time3) / (time2 - time1) should be < ((size4 - size3).toDouble / (size2 - size1) * slack)
    }
  }
}
