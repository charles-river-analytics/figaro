package com.cra.appril.birdmigration.elements

import scala.annotation.tailrec
import com.cra.figaro.language._
import annotation.tailrec
import collection.immutable.SortedSet
import com.cra.figaro.algorithm.AbstractionScheme

object CounterAbstraction extends AbstractionScheme[Int] {
  /**
   * Produce a sorted set of abstract points from the given concrete points by dividing the range of the
   * concrete points into equal sized bins.
   */

  //For the bird problem, we know there is a limit to the number of birds which can appear in a given cell.

  def select(concretePoints: Seq[Int], numAbstractPoints: Int): SortedSet[Int] = {
    if (concretePoints.isEmpty) {
      //println("early exit.")
      SortedSet.empty[Int]
    } else if (concretePoints.size < numAbstractPoints) {
      val result = SortedSet(concretePoints: _*)
      // println(result)
      result
    } else {
      //Use for 1000 bird
      val max = 1000
      val min = 0
      //val max = 6
      //val min = 0
      //val min = (Int.MaxValue /: concretePoints)(_ min _)
     // val max = (Int.MinValue /: concretePoints)(_ max _)
      val step = (max - min) / numAbstractPoints
      val start = min
      // val start = min + step / 2
      val result = SortedSet((for { i <- 0 until numAbstractPoints } yield start + step * i): _*)
      //println(result)
      result
    }

  }

  /**
   * Map a concrete Int to the closest abstract point. This method assumes the abstract points are
   * sorted. This is guaranteed if RegularDiscretization.select was used to create the abstract points.
   */
  def map(concretePoint: Int, abstractPoint: Set[Int]): Int = {
    @tailrec
    def helper(point: Int, choices: List[Int], prevPoint: Int): Int =
      choices match {
        case first :: rest =>
          if (point > first) helper(point, rest, first)
          else if (first - point < point - prevPoint) first
          else prevPoint
        case List() => prevPoint
      }

    abstractPoint.toList match {
      case first :: rest =>
        if (concretePoint <= first) first
        else helper(concretePoint, rest, first)
      case List() => throw new IllegalArgumentException("Empty list of abstract points")
    }
  }
}
