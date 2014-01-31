/*
 * AbstractionScheme.scala
 * Schemes to produce abstractions of elements to a smaller set of values.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

import com.cra.figaro.language._
import annotation.tailrec
import collection.immutable.SortedSet

/**
 * Trait of functions that map concrete points to abstract points.
 */
trait PointMapper[T] {
  /**
   * Map a concrete point to one of the given abstract points.
   */
  def map(concretePoint: T, abstractPoints: Set[T]): T
}

object PointMapper {
  /**
   * Default mapping function that maps each point to itself.
   */
  implicit def defaultMapper[T]: PointMapper[T] = new PointMapper[T] {
    def map(concretePoint: T, abstractPoints: Set[T]) = concretePoint
  }
}

/**
 * In an abstraction, the values of an element are reduced to a smaller, abstract set of values. Each
 * concrete value is mapped to a specific abstract value. Algorithms might choose to operate on the abstract
 * values rather than the concrete values.
 *
 * Elements that undergo an abstraction use an abstraction scheme to govern it. The abstraction scheme
 * consists of two methods. The first, select, determines the representative points for the abstraction to
 * which all points are mapped. The second, map, maps a point to one of the representative points.
 *
 * The map method is actually included in the PointMapper trait that AbstractionScheme extends. All elements
 * have a PointMapper, not just those that undergo an abstraction. This can be used in the implementation of
 * algorithms (see, e.g., Factor.scala). The implicit PointMapper is one that maps every point to itself, so
 * it can safely be used for every element that does not undergo an abstraction.
 *
 * Probably the most common type of abstraction is discretization of continuous elements. 
 * Currently the RegularDiscretization scheme is implemented, which uses evenly space abstract point.
 * There is an implicit abstraction scheme for Double elements using RegularDiscretization 
 * so one does not have to be specified explicitly.
 */

trait AbstractionScheme[T] extends PointMapper[T] {
  /**
   * Select a set of abstract points from the given set of concrete points.
   */
  def select(concretePoints: Seq[T], numAbstractPoints: Int): Set[T]
}

object AbstractionScheme {
  implicit val regularDiscretization: AbstractionScheme[Double] = RegularDiscretization

  /**
   * Default abstraction scheme for Double elements divides the range of an element into equal sized bins.
   */
  object RegularDiscretization extends AbstractionScheme[Double] {
    /**
     * Produce a sorted set of abstract points from the given concrete points by dividing the range of the
     * concrete points into equal sized bins.
     */
    def select(concretePoints: Seq[Double], numAbstractPoints: Int): SortedSet[Double] = {
      val min = (Double.MaxValue /: concretePoints)(_ min _)
      val max = (Double.MinValue /: concretePoints)(_ max _)
      val step = (max - min) / numAbstractPoints
      val start = min + step / 2
      SortedSet((for { i <- 0 until numAbstractPoints } yield start + step * i): _*)
    }

    /**
     * Map a concrete Double to the closest abstract point. 
     */
    def map(concretePoint: Double, abstractPoint: Set[Double]): Double = {
      @tailrec
      def helper(point: Double, choices: List[Double], prevPoint: Double): Double =
        choices match {
          case first :: rest =>
            if (point > first) helper(point, rest, first)
            else if (first - point < point - prevPoint) first
            else prevPoint
          case List() => prevPoint
        }

      abstractPoint.toList.sorted match {
        case first :: rest =>
          if (concretePoint <= first) first
          else helper(concretePoint, rest, first)
        case List() => throw new IllegalArgumentException("Empty list of abstract points")
      }
    }
  }
}

