/*
 * Factor.scala
 * General trait for factors over values.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.factored

import scala.annotation.tailrec
import com.cra.figaro.algorithm.lazyfactored.Extended

/**
 * Refactored by
 * 
 * @author Glenn Takata Sep 11, 2014
 *
 */
trait Factor[T] {
  def variables: List[Variable[_]]
  lazy val numVars = variables.size

  protected[figaro] var contents: Map[List[Int], T] = Map()

  val size = (1 /: variables)(_ * _.size)

  override def toString = "Factor(" + variables.map(_.id).mkString(",") + " " + contents.mkString(",") + ")"

  def hasStar = (false /: variables)(_ || _.valueSet.hasStar)

  def isEmpty = size == 0

    /**
   * Fold the given function through the contents of the factor, beginning with the given initial values
   */
  def foldLeft(initial: T, fn: (T, T) => T): T = {
    (initial /: contents.values)(fn(_, _))
  }
  
  /**
   * Returns the indices lists corresponding to all the rows in order.
   */
  def allIndices: List[List[Int]] = {
    @tailrec def helper(current: List[Int], accum: List[List[Int]]): List[List[Int]] =
      nextIndices(current) match {
        case Some(next) => helper(next, current :: accum)
        case None => (current :: accum).reverse
      }
    if (isEmpty) List()
    else helper(firstIndices, List())
  }

  /**
   * Returns the list of indices into the variable ranges associated with the first row in the factor.
   */
  def firstIndices: List[Int] = List.fill(numVars)(0)

  /**
   * Set the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined.
   */
  def set(indices: List[Int], value: T): Factor[T] = {
    contents += indices -> value
    this
  }

  /**
   * Get the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined.
   */
  def get(indices: List[Int]): T = contents(indices)

  /**
   * Given a list of indices corresponding to a row in the factor, returns the list of indices
   * corresponding to the next row.
   * Returns None if the last index list has been reached.
   */
  def nextIndices(indices: List[Int]): Option[List[Int]] = {
    def makeNext(position: Int) =
      for { i <- 0 until numVars } yield if (i > position) indices(i)
      else if (i < position) 0
      else indices(position) + 1
    def helper(position: Int): Option[List[Int]] =
      if (position == numVars) None
      else if (indices(position) < variables(position).size - 1) Some(makeNext(position).toList)
      else helper(position + 1)
    helper(0)
  }

  def fillByRule(rule: List[Extended[_]] => T):Factor[T]
  
  def mapTo[U](fn: T => U, variables: List[Variable[_]]): Factor[U]
  
  def product(    
    that: Factor[T],
    semiring: Semiring[T]): Factor[T]
  
  def combination(
    that: Factor[T],
    op: (T, T) => T): Factor[T]
  
  def sumOver(variable: Variable[_], semiring: Semiring[T]): Factor[T]
  
  def recordArgMax[U](variable: Variable[U], comparator: (T, T) => Boolean): Factor[U]
  
  def marginalizeTo(
    semiring: Semiring[T],
    targets: Variable[_]*): Factor[T]
  
  def deDuplicate(): Factor[T]
  
  def toReadableString: String
}

object Factor {
  /**
   * The mutliplicative identity factor.
   */
  def unit[T](semiring: Semiring[T]): Factor[T] = {
    val result = new BasicFactor[T](List())
    result.set(List(), semiring.one)
    result
  }
}