/*
 * UtilTest.scala   
 * Test of utility functions. 
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
import com.cra.figaro.util._
import com.cra.figaro.test._
import com.cra.figaro.test.tags.Performance

class UtilTest extends WordSpec with Matchers {
  "Util.round" when {
    "given a number that is less than the lowest point" should {
      "return the lowest point" in {
        round(0.0, List(0.1, 0.3)) should equal(0.1)
      }
    }

    "given a number that is between the lowest and highest point" should {
      "return the nearest point" in {
        round(0.2, List(0.1, 0.14, 0.21, 0.3)) should equal(0.21)
      }
    }

    "given a number that is greater than the highest point" should {
      "return the highest point" in {
        round(0.4, List(0.1, 0.3)) should equal(0.3)
      }
    }
  }

  "Util.normalize" when {
    "given a list with positive total value" should {
      "produce a result that sums to 1" in {
        val n = normalize(List(0.2, 0.3))
        (0.0 /: n)(_ + _) should be(1.0 +- 0.00000001)
      }

      "maintain the proportions between the values" in {
        val n = normalize(List(0.2, 0.3))
        n(1) / n(0) should be(1.5 +- 0.00000001)
      }
    }

    "given a list with zero total value" should {
      "throw a ZeroTotalUnnormalizedProbabilityException" in {
        an [ZeroTotalUnnormalizedProbabilityException] should be thrownBy { normalize(List(0.0, 0.0)) } 
      }
    }
  }

  "Util.selectMultinomial" when {
    "given an index that is less than the sum of the probabilities" should {
      "select the correct item according to the index" in {
        val m = List(0.2 -> 'foo, 0.3 -> 'bar, 0.5 -> 'baz)
        selectMultinomial(0.4, m) should equal('bar)
      }
    }

    "given an index that is greater than the sum of the probabilities" should {
      "throw InvalidMultinomialIndexException" in {
        val m = List(0.2 -> 'foo, 0.3 -> 'bar, 0.5 -> 'baz)
        an [InvalidMultinomialIndexException] should be thrownBy { selectMultinomial(1.2, m) } 
      }
    }
  }

  "Util.sampleMultinomial" when {
    "given probabilities that sum to 1" should {
      "select an item with frequency equal to its probability" in {
        val m = List(0.2 -> 'foo, 0.3 -> 'bar, 0.5 -> 'baz)
        var numTrials = 100000
        var successes = 0
        for { i <- 1 to numTrials } { if (sampleMultinomial(m) == 'bar) successes += 1 }
        successes.toDouble / numTrials should be(0.3 +- 0.01)
      }
    }

    "given probabilities that sum to less than 1" should {
      "sometimes throw LessThanOneTotalProbabilityException" in {
        val m = List(0.2 -> 'foo, 0.3 -> 'bar, 0.4 -> 'baz)
        try {
          for { i <- 1 to 100000 } { sampleMultinomial(m) }
          assert(false, "no exception thrown")
        } catch {
          case _: LessThanOneTotalProbabilityException => ()
          case e: Throwable => assert(false, "incorrect exception " + e + " thrown")
        }
      }
    }
  }

  "Util.cartesianProduct" when {
    "given zero inputs" should {
      "return a list with a single empty element" in {
        cartesianProduct() should equal(List(List()))
      }
    }

    "given two inputs " should {
      "return a list of two-element lists that contains every combination of values of its inputs and no others" in {
        val s = List(1, 2)
        val l = List('a', 'b', 'c')
        val c = cartesianProduct(s, l)
        c should equal(List(List(1, 'a'), List(1, 'b'), List(1, 'c'), List(2, 'a'), List(2, 'b'), List(2, 'c')))
      }
    }
  }

  "Util.upperTriangle" when {
    "given a list of at least two elements" should {
      "return all pairs of elements in the list in which the first precedes the second" in {
        upperTriangle(List(1, 2, 3)) should equal(List((1, 2), (1, 3), (2, 3)))
      }
    }

    "given a list of one element" should {
      "return the empty list" in {
        upperTriangle(List(1)) should equal(List())
      }
    }
  }

  "Util.indices" when {
    "given a traversable and an element" should {
      "return a list containing all indices of the element in the traversable and no others" in {
        val l: Traversable[Int] = List(1, 2, 3, 1, 4, 5, 1)
        indices(l, 1) should equal(List(0, 3, 6))
        indices(l, 0) should equal(List())
      }
    }
  }

  "Util.insertAtIndices" when {
    "given a traversable, a value, and a list of indices in order" should {
      "return a traversable that contains all elements of the original traversable with the given value inserted at " +
        "the given indices" in {
          val t: Traversable[Int] = List(1, 2)
          insertAtIndices(t, List(0, 2, 4), 3).toList should equal(List(3, 1, 3, 2, 3))
        }
    }

    "given a traversable, a value, and a set of indices such that some indices are too large" should {
      "throw IllegalArgumentException" in {
        an [IllegalArgumentException] should be thrownBy { insertAtIndices(List(1, 2), List(0, 2, 5), 3) } 
      }
    }
  }

  "Util.memo" when {
    "applied once to a function" should {
      "return a function that produces the same value as the given function" in {
        def f(x: Int) = x + 1
        val mf = memo(f)
        mf(7) should equal(8)
        mf(9) should equal(10)
      }

      "return a function that produces the same value as the given function with two arguments" in {
        def f(x: Int, y: Int) = x + y
        def g = (pair: (Int, Int)) => f(pair._1, pair._2)
        val mf = memo(g)
        mf((7, 8)) should equal(15)
        mf((8, 9)) should equal(17)
      }

      "return a function that only calls the memoized function once if applied repeatedly to the same argument" in {
        var count = 0
        def f(x: Int) = {
          count += 1
          x + 1
        }
        val mf = memo(f _)
        mf(7)
        mf(7)
        count should equal(1)
        mf(9)
        count should equal(2)
      }

      "return a function that only calls the memoized function once if applied repeatedly to the same two arguments" in {
        var count = 0
        def f(x: Int, y: Int) = {
          count += 1
          x + y
        }
        def g = (pair: (Int, Int)) => f(pair._1, pair._2)
        val mf = memo(g)
        mf(7, 8)
        mf(7, 8)
        count should equal(1)
        mf(8, 9)
        count should equal(2)
      }

    }

    "applied twice to a function" should {
      "return two functions that each call the memoized function, even if applied to the same argument" in {
        var count = 0
        def f(x: Int) = {
          count += 1
          x + 1
        }
        val mf = memo(f _)
        val nf = memo(f _)
        mf(7)
        nf(7)
        count should equal(2)
      }

      "return two functions that each call the memoized function, even if applied to the same two arguments" in {
        var count = 0
        def f(x: Int, y: Int) = {
          count += 1
          x + y
        }
        def g = (pair: (Int, Int)) => f(pair._1, pair._2)
        val mf = memo(g)
        val nf = memo(g)
        mf(7, 8)
        nf(7, 8)
        count should equal(2)
      }

    }
  }

  "Util.reachable" when {
    "given a graph that maps items to traversables of items, and an element" should {
      "return a traversable containing the elements reachable from the given element and no others" in {
        val graph = Map(1 -> List(2, 3), 2 -> List(3, 4), 3 -> List(5), 4 -> List(), 5 -> List(), 6 -> List(1))
        val r = reachable(1, graph).toList
        r should contain(2)
        r should contain(3)
        r should contain(4)
        r should contain(5)
        r should not contain (1)
        r should not contain (6)
      }

      "return a traversable that does not contain duplicates" in {
        val graph = Map(1 -> List(2, 3), 2 -> List(3, 4), 3 -> List(5), 4 -> List(), 5 -> List(), 6 -> List(1))
        val r = reachable(1, graph).toList
        r should equal(r.distinct)
      }

      "take time roughly linear in the number of edges of the graph" taggedAs (Performance) in {
        val graph1 = Map(
          1 -> List(2, 3),
          2 -> List(4, 5), 3 -> List(4, 5),
          4 -> List(6, 7), 5 -> List(6, 7),
          6 -> List(8), 7 -> List(8),
          8 -> List())
        val numEdges1 = 12

        val graph2 = Map(
          1 -> List(2, 3),
          2 -> List(4, 5), 3 -> List(4, 5),
          4 -> List(6, 7), 5 -> List(6, 7),
          6 -> List(8, 9), 7 -> List(8, 9),
          8 -> List(10, 11), 9 -> List(10, 11),
          10 -> List(12, 13), 11 -> List(12, 13),
          12 -> List(14, 15), 13 -> List(14, 15),
          14 -> List(16), 15 -> List(16),
          16 -> List())
        val numEdges2 = 28

        def reach(graph: Map[Int, List[Int]])() =
          reachable(1, graph)

        val time1 = measureTime(reach(graph1), 20, 100)
        val time2 = measureTime(reach(graph2), 20, 100)
        val slack = 1.2
        time2 / time1 should be < (numEdges2.toDouble / numEdges1 * slack)
      }

      "produce a result of the same type as its input" in {
        val graph1 = Map(1 -> List(1))
        reachable(1, graph1).isInstanceOf[List[_]] should be(true)
        val graph2 = Map(1 -> Set(1))
        reachable(1, graph2).isInstanceOf[Set[_]] should be(true)
      }
    }

    "given a cyclic graph and an element involved in a cycle" should {
      "terminate and return a traversable includeing the element" in {
        val graph = Map(1 -> List(1))
        reachable(1, graph).toList should contain(1)
      }
    }
  }

  "Util.argmax" should {
    "return the index of the largest element in a list" in {
      argmax(List(1.0)) should equal(0)
      argmax(List(1.0, 2.0)) should equal(1)
      argmax(List(1.0, 3.0, 2.0)) should equal(1)
      argmax(List(4.0, 1.0, 3.0, 2.0)) should equal(0)
    }
  }
}
