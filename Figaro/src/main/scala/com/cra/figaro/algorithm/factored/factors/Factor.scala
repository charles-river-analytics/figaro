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
package com.cra.figaro.algorithm.factored.factors

import scala.annotation.tailrec
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.lazyfactored.Extended

/**
 * Definition of Factor <p>
 *
 * A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. Factors are parameterized by the
 * Variables they contain. Parent variables are distinguished from the output variable.
 *
 * Refactored by
 *
 * @author Glenn Takata Sep 11, 2014
 *
 */
trait Factor[T] {
  def parents: List[Variable[_]]
  def output: List[Variable[_]]
  def variables = parents ::: output

  lazy val numVars = variables.size

  protected[figaro] var contents: Map[List[Int], T] = Map()

  val size = (1 /: variables)(_ * _.size)

  /**
   * Description that includes the variable list and conditional probabilites
   */
  override def toString = "Factor(" + variables.map(_.id).mkString(",") + " " + contents.mkString(",") + ")"

  /**
   * Indicates if any of this Factor's variables has Star
   */
  def hasStar = (false /: variables)(_ || _.valueSet.hasStar)

  /**
   * Indicates if this Factor has any variables
   */
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
      for { i <- 0 until numVars } yield if (i < position) indices(i)
      else if (i > position) 0
      else indices(position) + 1
    def helper(position: Int): Option[List[Int]] =
      if (position < 0) None
      else if (indices(position) < variables(position).size - 1) Some(makeNext(position).toList)
      else helper(position - 1)
    helper(numVars - 1)
  }

  /**
   * Fill the contents of this factor by applying a rule to every combination of values.
   */
  def fillByRule(rule: List[Extended[_]] => T): Factor[T]

  /**
   * Fill the contents of the target by applying the given function to all elements of this factor.
   */
  def mapTo[U](fn: T => U): Factor[U]

  /**
   * Returns the product of this factor with another factor according to a given multiplication function.
   * The product is associated with all variables in the two inputs, and the value associated with an assignment
   * is the product of the values in the two inputs.
   */
  def product(
    that: Factor[T],
    semiring: Semiring[T]): Factor[T]

  /**
   * Generic combination function for factors. By default, this is product, but other operations
   * (such as divide that is a valid operation for some semirings) can use this
   */
  def combination(
    that: Factor[T],
    op: (T, T) => T): Factor[T]

  /**
   * Returns the summation of the factor over a variable according to an addition function.
   * The result is associated with all the variables in the
   * input except for the summed over variable and the value for a set of assignments is the
   * sum of the values of the corresponding assignments in the input.
   */
  def sumOver(variable: Variable[_], semiring: Semiring[T]): Factor[T]

  /**
   * Returns a factor that maps values of the other variables to the value of the given variable that
   * maximizes the entry associated with that value, according to some maximization function.
   * comparator defines the maximization. It returns true iff its second argument is greater than its first.
   *
   * @tparam U The type of element whose value is being recorded. The resulting factor maps values of
   * other variables in this factor to this type.
   * @tparam T The type of entries of this factor.
   */
  def recordArgMax[U](variable: Variable[U], comparator: (T, T) => Boolean): Factor[U]

  /**
   * Returns the marginalization of the factor to a variable according to the given addition function.
   * This involves summing out all other variables.
   */
  def marginalizeTo(
    semiring: Semiring[T],
    targets: Variable[_]*): Factor[T]

  /**
   * Returns a new Factor with duplicate variable(s) removed
   */
  def deDuplicate(): Factor[T]

  /**
   * Creates a new Factor of the same class with a different type
   */
  def convert[U](): Factor[U]

  /**
   * Produce a readable string representation of the factor
   */
  def toReadableString: String

}