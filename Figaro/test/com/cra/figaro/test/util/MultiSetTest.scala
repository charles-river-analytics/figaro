/*
 * MultiSetTest.scala   
 * 
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 */

package com.cra.figaro.test.util

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.cra.figaro.util._

class MultiSetTest extends WordSpec with ShouldMatchers {
  "A hashing multiset" should {
    "contain an element the correct number of times after multiple addOnes" in {
      val ms = HashMultiSet[Int]()
      ms.addOne(5)
      ms.addOne(5)
      ms.addOne(6)
      ms(5) should equal(2)
      ms(6) should equal(1)
      ms(7) should equal(0)
      ms.counts.size should equal(2)
    }

    "contain an element the correct number of times after addMany" in {
      val ms = HashMultiSet[Int]()
      ms.addMany(5, 2)
      ms.addMany(6, 1)
      ms(5) should equal(2)
      ms(6) should equal(1)
      ms(7) should equal(0)
      ms.counts.size should equal(2)
    }

    "contain an element the correct number of times after multiple removeOnes" in {
      val ms = HashMultiSet[Int]()
      ms.addMany(5, 4)
      ms.addMany(6, 1)
      ms.removeOne(5)
      ms.removeOne(5)
      ms.removeOne(6)
      ms(5) should equal(2)
      ms(6) should equal(0)
      ms(7) should equal(0)
      ms.counts.size should equal(1)
    }

    "contain an element the correct number of times after removeMany" in {
      val ms = HashMultiSet[Int]()
      ms.addMany(5, 4)
      ms.addMany(6, 1)
      ms.removeAll(5)
      ms(5) should equal(0)
      ms(6) should equal(1)
      ms(7) should equal(0)
      ms.counts.size should equal(1)
    }

    "produce the right list of elements" in {
      val ms = HashMultiSet[Int]()
      ms.addMany(5, 2)
      ms.addMany(6, 1)
      val l = ms.elements
      l.length should equal(3)
      l.count(_ == 5) should equal(2)
      l.count(_ == 6) should equal(1)
    }

    "produce the right behavior under foreach" in {
      val ms = HashMultiSet[Int]()
      ms.addMany(5, 2)
      ms.addMany(6, 1)
      var x = 0
      def f(i: Int) { x += i }
      ms.foreach(f)
      x should equal(16)
    }

    "produce the right result under map" in {
      val ms = HashMultiSet[Int]()
      ms.addMany(5, 2)
      ms.addMany(6, 1)
      val ms2 = ms.map((i: Int) => i.toString)
      ms2("5") should equal(2)
      ms2("6") should equal(1)
      ms2("7") should equal(0)
      ms2.counts.size should equal(2)
    }

    "produce the correct union" in {
      val ms1 = HashMultiSet[Int]()
      ms1.addMany(5, 2)
      ms1.addMany(6, 1)
      val ms2 = HashMultiSet[Int]()
      ms1.addMany(5, 3)
      ms1.addMany(7, 4)
      val ms3 = ms1 union ms2
      ms3(5) should equal(5)
      ms3(6) should equal(1)
      ms3(7) should equal(4)
      ms3.counts.size should equal(3)
    }
  }
}
