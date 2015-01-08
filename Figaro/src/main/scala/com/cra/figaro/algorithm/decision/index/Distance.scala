/*
 * Distance.scala
 * Trait for distances in decisions
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.decision.index

import scala.language.implicitConversions

/**
 * Trait Distance
 *
 * Classes that are used as decision parents in the approximate decision strategies
 * (i.e., they use Nearest Neighbor querying to compute optimal decisions) must
 * implement the Distance trait. This is so that the distance between two parent values
 * can be easily computed.
 *
 * For the built in classes Double, Int and Boolean, implicit conversions to Distance
 * are already provided, and the user does not need to do anything. The default also
 * handles two tuple parents of the same base type using the L2 distance
 *
 * For any user defined types as a parent, they must implement the Distance trait,
 * and define the function distance(that:T) which defines how to compute the distance
 * between two instances of the class. If a user uses any combination of tuples other
 * than 2-tuples of Int, Boolean or Double, they must define the tuple distance as well.
 * See TupleDistance2 below for an example.
 *
 */
trait Distance[T] {
  /**
   * Return the distance from this value to another.
   */
  def distance(that: T): Double
}

/**
 * Trait to compute the L-2 Norm.
 */
trait L2Norm {
  /**
   * Reduce a variable list of doubles using the L2 norm.
   */
  def reduce(v: Double*) = math.pow((0.0 /: v)((c, n) => c + math.pow(math.abs(n), 2.0)), 0.5)
}

/**
 * Abstract class to define the distance between tuples of classes that implement the Distance[T] trait.
 */
abstract class TupleDistance {
  /**
   * Reduce the set of doubles corresponding to the distance between each pair of components to a
   * single double representing the distance between the tuples.
   */
  def reduce(v: Double*): Double
}

/**
 * Extension of Ints to the Distance[T] trait. Computes the L1 distance between two Ints.
 */
private[index] case class IntDistance(value: Int) extends Distance[Int] {
  def distance(that: Int) = math.abs(value - that)
}

/**
 * Extension of Doubles to the Distance[T] trait. Computes the L1 distance between two Doubles.
 */
private[index] case class DoubleDistance(value: Double) extends Distance[Double] {
  def distance(that: Double) = math.abs(value - that)
}

/**
 * Extension of Boolean to the Distance[T] trait. Returns the (this xor that).
 */
private[index] case class BooleanDistance(value: Boolean) extends Distance[Boolean] {
  def distance(that: Boolean) = if (value ^ that) 1.0 else 0.0
}

/**
 * TupleDistance2 defines the distance between tuples of two values (not necessarily of the
 * same class). It extends the TupleDistance class, which defines a reduce() function
 * that determines how to combine the distances of the tuple. For instance, if you have
 * a parent tuple of (double, boolean), then the distance between the double and boolean
 * portions of the tuple are computed using the Distance trait, and the user needs to define
 * reduce() to determine how to combine distances from a double and boolean together to
 * create a single distance value.
 *
 * 
 * @param value The tuple to compute the distance to.
 */
case class TupleDistance2[T1 <% Distance[T1], T2 <% Distance[T2]](value: (T1, T2))
  extends TupleDistance with Distance[(T1, T2)] with L2Norm {
  def distance(that: (T1, T2)) = reduce(value._1.distance(that._1), value._2.distance(that._2))
}

/** 
 *  Contains implicit conversions from Int, Double and Boolean to classes that extend the Distance[T] trait,
 *  as well as 2-tuples of Int, Double and Boolean.
 *  
 *  Uses L1 distance for single values and L2 distance for tuples.
 */
object Distance {
  implicit def int2Dist(x: Int) = IntDistance(x)
  implicit def double2Dist(x: Double) = DoubleDistance(x)
  implicit def boolean2Dist(x: Boolean) = BooleanDistance(x)

  implicit def int2DistTuple(x: (Int, Int)) = TupleDistance2[Int, Int](x)
  implicit def double2DistTuple(x: (Double, Double)) = TupleDistance2[Double, Double](x)
  implicit def boolean2DistTuple(x: (Boolean, Boolean)) = TupleDistance2[Boolean, Boolean](x)
}
