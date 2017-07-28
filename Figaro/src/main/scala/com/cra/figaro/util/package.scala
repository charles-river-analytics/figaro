/*
 * package.scala 
 * Utility functions for Figaro.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro

import scala.annotation.tailrec
import scala.collection.mutable.Map

package object util {
  class InvalidDistributionException extends RuntimeException

  class NegativeProbabilityException extends InvalidDistributionException

  class ZeroTotalUnnormalizedProbabilityException extends InvalidDistributionException

  class LessThanOneTotalProbabilityException extends InvalidDistributionException

  class InvalidMultinomialIndexException extends RuntimeException

  /**
   * A random number generator.
   */

  def setSeed(s: Long): Unit = { seed = s }

  def getSeed(): Long = { seed }
  private var nextHashCode = 0

  def getNextHashCode: Int = {
    nextHashCode += 1
    nextHashCode %= Int.MaxValue - 1
    nextHashCode
  }

  var seed = System.currentTimeMillis()

  lazy val random = new scala.util.Random(seed)

  /**
   * Computes and returns the argument, timing how long it takes to produce the answer and printing
   * the result.
   */
  def timed[T](arg: => T, title: String): T = {
    val time0 = System.nanoTime()
    val result = arg
    val time1 = System.nanoTime()
    val elapsed = (time1 - time0).toDouble / 1000000000
    println(title + " time: " + elapsed)
    result
  }

  /**
   * Computes the average time to run the given function.
   * The second argument is the number of times to run it before
   * starting to measure to warm up the JVM. The third argument is the number of times to run and measure.
   */
  def measureTime[T](f: () => T, warmup: Int, runs: Int): Double = {
    for { i <- 1 to warmup } f()
    val time0 = System.nanoTime()
    for { i <- 1 to runs } f()
    val time1 = System.nanoTime()
    val elapsed = (time1 - time0).toDouble / 1000000000
    elapsed / runs
  }

  /**
   * Round the given double to the nearest element of the non-empty ordered list.
   */
  def round(target: Double, points: Iterable[Double]): Double = {
    def helper(previous: Double, diff: Double, points: Iterable[Double]): Double =
      if (points.nonEmpty) {
        val first = points.head
        if (first < target) helper(first, target - first, points.tail)
        else if (first - target < diff) first
        else previous
      } else previous

    val first = points.head
    if (target < first) first
    else helper(first, target - first, points.tail)
  }

  /**
   * Normalize the given list of doubles so that the proportions remain constant and they sum to 1.
   * An exception will be thrown if the inputs sum to 0.
   */
  def normalize(unnormalized: List[Double]): List[Double] = {
    val normalizer = (0.0 /: unnormalized)(_ + _)
    if (normalizer == 0.0) throw new ZeroTotalUnnormalizedProbabilityException
    unnormalized map (_ / normalizer)
  }

  /**
   * Sample a value given a multinomial distribution. The input is a list of pairs where each pair
   * specifies a value and its associated probability. This method assumes the probabilities sum to
   * 1.
   */
  def sampleMultinomial[T](clauses: List[(Double, T)]): T =
    try { selectMultinomial(random.nextDouble(), clauses) } catch {
      case _: InvalidMultinomialIndexException => throw new LessThanOneTotalProbabilityException
    }

  /*
   * Select an element of a multinomial using a Double index. 
   */
  private[figaro] def selectMultinomial[T](index: Double, clauses: List[(Double, T)]): T = {
    clauses match {
      case (firstProb, firstResult) :: restClauses =>
        if (index < firstProb) firstResult
        else selectMultinomial(index - firstProb, restClauses)
      case Nil => throw new InvalidMultinomialIndexException
    }
  }
  /**
   * Returns the Cartesian product of any number of inputs. The results are returned in
   * lexicographic order.
   */
  def cartesianProduct(args: List[_]*): List[List[Any]] =
    args.toList match {
      case List() => List(List())
      case arg1 :: rest => arg1 flatMap (x => cartesianProduct(rest: _*) map (y => x :: y))
    }

  /**
   * Returns the Cartesian product of any number of inputs of the same type. The results are returned in
   * lexicographic order.
   */
  def homogeneousCartesianProduct[T](args: List[T]*): List[List[T]] =
    args.toList match {
      case List() => List(List())
      case arg1 :: rest => arg1 flatMap (x => homogeneousCartesianProduct(rest: _*) map (y => x :: y))
    }

  /**
   * Computes all pairs of elements in the list in which the first element appears before the second.
   */
  def upperTriangle[T](list: List[T]): List[(T, T)] =
    list match {
      case x :: xs => (xs map ((y: T) => (x, y))) ::: upperTriangle(xs)
      case _ => List()
    }

  /**
   * Returns all indices of the given element in the traversable.
   */
  def indices[T](traversable: Traversable[T], x: T): List[Int] = {
    def helper(position: Int, remaining: Traversable[T]): List[Int] =
      if (remaining.isEmpty) List()
      else if (remaining.head == x) position :: helper(position + 1, remaining.tail)
      else helper(position + 1, remaining.tail)
    helper(0, traversable)
  }

  /**
   * Given a traversable, some indices, and a value,
   * inserts the value at the given indices into the traversable.
   * The resulting list contains all the elements of the input traversable as well as the value at the appropriate
   * indices.
   * For example, insertAtIndices(List(1,2), List(0,2,4), 3) yields List(3,1,3,2,3).
   * The indices must be in order to produce the correct result.
   * If the indices are such that there would be a gap in the resulting traversable,
   * IllegalArgumentException is thrown.
   */
  def insertAtIndices[T](traversable: Traversable[T], indices: List[Int], value: T): List[T] = {
    def helper(position: Int, remainingTraversable: Traversable[T], remainingIndices: List[Int]): List[T] =
      remainingIndices match {
        case Nil => remainingTraversable.toList
        case index :: rest if index == position => value :: helper(position + 1, remainingTraversable, rest)
        case index :: rest =>
          if (remainingTraversable.nonEmpty)
            remainingTraversable.head :: helper(position + 1, remainingTraversable.tail, remainingIndices)
          else throw new IllegalArgumentException("Invalid indices " + indices.mkString(", "))
      }
    helper(0, traversable, indices)
  }

  /**
   * Find the elements that are reachable from given elements in a directed graph. The implementation avoids following
   * the exponential number of paths by marking which nodes have been checked. This technique also allows the algorithm
   * to work on cyclic graphs.
   * @param graph A function that given an element returns a collection of nodes to which there is a directed edge.
   * @param includeStart Flag to indicate whether to explicitly include the start elements in the returned set or not.
   * @param start Graph elements from which to begin searching.
   * @return A set containing all nodes reachable by directed edges from the start nodes. Note: if a start node is
   * reachable by a path from another start node (or by a cycle containing itself), the node will be included the set
   * even if `includeStart` is set to false.
   */
  def reachable[T](graph: T => Traversable[T], includeStart: Boolean, start: T*): Set[T] = {
    var marked: Set[T] = if(includeStart) start.toSet else Set()

    def helper(t: T): Unit =
      if (!marked.contains(t)) {
        marked += t
        for (child <- graph(t)) helper(child)
      }

    for(t <- start ; child <- graph(t)) helper(child)

    marked
  }

  /**
   * Memoize the given function so that it is only applied once to each input with the result stored.
   */
  def memo[T, U](fn: T => U): T => U = {
    val cache: Map[T, U] = Map()
    (t: T) =>
      cache.get(t) match {
        case Some(u) => u
        case None =>
          val result = fn(t)
          cache += t -> result
          result
      }
  }
  
  /**
   * Gets the value associated with a key in a map, inserting a default value if it is not found.
   * The default is only evaluated if the key is not found in the map.
   */
  def getOrElseInsert[T,U](map: Map[T,U], key: T, default: => U): U = {
    map.get(key) match {
      case Some(value) => value
      case None =>
        val result = default // avoid evaluating call by name default multiple times
        map += key -> result
        result
    }
  } 

  /**
   * Finds the index of the maximal element in the sequence.
   */
  def argmax(seq: Seq[Double]): Int = {
    def helper(l: List[Double], currentIndex: Int, bestIndexSoFar: Int, bestSoFar: Double): Int =
      l match {
        case List() => bestIndexSoFar
        case first :: rest =>
          if (first > bestSoFar) helper(rest, currentIndex + 1, currentIndex, first)
          else helper(rest, currentIndex + 1, bestIndexSoFar, bestSoFar)
      }
    if (seq.isEmpty) throw new IllegalArgumentException("Empty list")
    else helper(seq.tail.toList, 1, 0, seq.head)
  }

  /**
   * Computes the difference of two probabilities in log space.
   * Returns NaN if p2>p1, -Infinity if p2=p1
   */
  def logDiff(p1: Double, p2: Double): Double = {
    // Also covers the case where p1 = p2 = -Infinity
    if(p2 == Double.NegativeInfinity) p1
    // log(exp(p1) - exp(p2)) = log(exp(p1) * (1 - exp(p2) / exp(p1))) = p1 + log(1 - exp(p2 - p1))
    else p1 + Math.log1p(-Math.exp(p2 - p1))
  }
  
  /**
   * Sums two probabilities in log space.
   */
  def logSum(p1: Double, p2: Double): Double = {
   logSumMany(List(p1, p2))
  }
  
  /**
   * Computes the sum of many probabilities in log space. 
   */
  def logSumMany(xs: Traversable[Double]): Double = {
    val max = xs.foldLeft(Double.NegativeInfinity)(_ max _)
    if (max == Double.NegativeInfinity) Double.NegativeInfinity
    else {
      var total = 0.0
      for (x <- xs) { total += Math.exp(x - max) }
      Math.log(total) + max
    }
  }

  /**
   * Returns logp if all conditions are true. Else returns -Inf.
   */
  def bound(logp: Double, conditions: Boolean*): Double =
    if (conditions.exists(!_))
      Double.NegativeInfinity
    else
      logp

  /**
   * Find the first element of the sequence where the predicate evaluates to true, assuming that the predicate evaluates
   * to false on indices `0 until n` and evaluates to true on indices `n until seq.length` for some `n`. Returns
   * `Some(t)` if there exists such an element, and `None` otherwise.
   */
  def binarySearch[T](seq: IndexedSeq[T], predicate: T => Boolean): Option[T] = {
    binarySearch(seq, predicate, 0, seq.length)
  }

  /**
   * Find the first element of the sequence slice from `from` to `to` where the predicate evaluates to true. Thus
   * assumes that the predicate evaluates to false on indices `from until n` and evaluates to true on indices
   * `n until to` for some `n`. Returns `Some(t)` if there exists such an element, and `None` otherwise.
   */
  @tailrec
  def binarySearch[T](seq: IndexedSeq[T], predicate: T => Boolean, from: Int, to: Int): Option[T] = {
    if(to == from) None
    else if(to == from + 1) {
      val result = seq(from)
      if(predicate(result)) Some(result)
      else None
    }
    else {
      val index = from + (to - from - 1) / 2
      if(predicate(seq(index))) binarySearch(seq, predicate, from, index + 1)
      else binarySearch(seq, predicate, index + 1, to)
    }
  }
}

