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
import scala.collection.Iterable
import scala.collection.mutable.ListBuffer
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.lazyfactored.Extended
import scala.reflect.runtime.universe._
/**
 * Definition of Factor <p>
 *
 * A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. 
 * 
 * Factors are parameterized by the type of the Variables they contain and contain a semiring
 * that defines the mathematical operation to be performed on the values 
 * Parent variables are distinguished from the output variable.
 *
 * Refactored by
 *
 * @author Glenn Takata Sep 11, 2014
 *
 */
trait Factor[T] {
  def semiring: Semiring[T]
  
  def parents: List[Variable[_]]
  def output: List[Variable[_]]
  def variables = parents ::: output

  lazy val numVars = variables.size

  protected[figaro] var contents: Map[List[Int], T] = Map()

  val size = (1 /: variables)(_ * _.size)

  /**
   * Creates a new factor of the same type
   */
  def createFactor[T](parents: List[Variable[_]], output: List[Variable[_]], _semiring: Semiring[T] = semiring): Factor[T]

  /**
   * Description that includes the variable list and conditional probabilities
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
   * Indicates if this Factor is condition/constrained
   */
  val isConstraint: Boolean = false

  def contains(index: List[Int]) = contents.contains(index)

  /**
   * Fold the given function through the contents of the factor, beginning with the given initial values
   */
  def foldLeft(initial: T, fn: (T, T) => T): T = {
    (initial /: contents.values)(fn(_, _))
  }

  def getIndices: Indices = generateIndices

  def generateIndices: Indices = new Indices(variables)
  
  def convertIndicesToValues(indices: List[Int]): List[Extended[_]] = {
    val values = for { i <- 0 until indices.size } yield variables(i).range(indices(i))
    values.toList
  }

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
  def get(indices: List[Int]): T

  /**
   * Fill the contents of this factor by applying a rule to every combination of values.
   */
  def fillByRule(rule: List[Extended[_]] => T): Factor[T]

  /**
   * Fill the contents of the target by applying the given function to all elements of this factor.
   */
  def mapTo[U](fn: T => U, _semiring: Semiring[U] = semiring): Factor[U]

  /**
   * Returns the product of this factor with another factor according to a given multiplication function.
   * The product is associated with all variables in the two inputs, and the value associated with an assignment
   * is the product of the values in the two inputs.
   */
  def product(
    that: Factor[T]): Factor[T]

  /**
   * Combination function for factors. By default, this is product, but other operations
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
   *
   * If no funciton is provided, this defaults to the sum function in this factor's semiring.
   */
  def sumOver(
    variable: Variable[_],
    sum: (T, T) => T = semiring.sum): Factor[T]

  /**
   * Returns a factor that maps values of the other variables to the value of the given variable that
   * maximizes the entry associated with that value, according to some maximization function.
   * comparator defines the maximization. It returns true iff its second argument is greater than its first.
   *
   * @tparam U The type of element whose value is being recorded. The resulting factor maps values of
   * other variables in this factor to this type.
   * @tparam T The type of entries of this factor.
   */
  def recordArgMax[U](variable: Variable[U], comparator: (T, T) => Boolean, _semiring: Semiring[U] = semiring.asInstanceOf[Semiring[U]]): Factor[U]

  /**
   * Returns the marginalization of the factor to a variable according to the addition
   * function in this factor's semiring. This involves summing out all other variables.
   */
  def marginalizeTo(targets: Variable[_]*): Factor[T] = marginalizeToWithSum(semiring.sum, targets:_*)

  /**
    * Returns the marginalization of the factor to a variable according to the given
    * addition function. Unlike marginalizeTo, this uses the provided sum function.
    * This is useful e.g. to easily switch between max-product and sum-product operations
    * when the data within are unchanged and the product operation is the same.
    *
    * The returned factor uses the semiring associated with this factor; it does not
    * override the sum function of the semiring with the function given here.
    */
  def marginalizeToWithSum(
    sum: (T, T) => T,
    targets: Variable[_]*): Factor[T]

  /**
   * Returns a new Factor with duplicate variable(s) removed
   */
  def deDuplicate(): Factor[T]

  /**
   * Produce a readable string representation of the factor
   */
  def toReadableString: String

}

class Indices(variables: List[Variable[_]]) extends Iterable[List[Int]] {
  val limits = variables.map(_.size.asInstanceOf[Int] - 1)
  val numVars = limits.length
  val factorSize = (1 /: variables)(_ * _.size)
  
  
  def iterator = new Iterator[List[Int]] {
    var current: List[Int] = List(-1)
    def hasNext = {
      current match {
        case List(-1) =>
          current = List.fill(numVars)(0)
          true
        case l: List[Int] =>
          nextIndices(l) match {
            case Some(i) =>
              current = i
              true
            case None => false
          }
        case _ => false
      }
    }

    def next: List[Int] = current
  }
  
  def allIndices: List[List[Int]] = {
    @tailrec def helper(current: List[Int], accum: List[List[Int]]): List[List[Int]] =
      nextIndices(current) match {
        case Some(next) => helper(next, current :: accum)
        case None => (current :: accum).reverse
      }
    if (factorSize == 0) List()
    else helper(List.fill(numVars)(0), List())
  }

  /**
   * Given a list of indices corresponding to a row in the factor, returns the list of indices
   * corresponding to the next row.
   * Returns None if the last index list has been reached.
   */  
  def nextIndices(indices: List[Int]): Option[List[Int]] = {
    var temp = indices.reverse
    var newIndices = List[Int]()
    var success = true
    var carry = true
    
    var position = numVars - 1
    
    while(carry) {
      temp match {
        case head :: tail => {
          //  current position can be incremented
          // stop here
          if (head < limits(position)) {
            newIndices ::= head + 1
            newIndices :::= tail.reverse
            carry = false
          }
          // limit reached so zero current position and 
          // carry to next
          else if (head == limits(position)) {
            newIndices ::= 0
            temp = tail
            position = position - 1
          }
        }

        case Nil => {
          success = false
          carry = false
        }
      }
    }
    
    if (success) {
      Some(newIndices)
    }
    else {
      None
    }
  }
}

object Factor {
//  def combineIndices(thisIndices: List[Int], thisIndexMap: List[Int], thatIndices: List[Int], thatIndexMap: List[Int], numVars: Int): Option[List[Int]] = {
//    var newIndices = new ListBuffer[Int]()
//    var good = true;
//
//    for (i <- 0 until numVars) {
//      val inThis = thisIndexMap.indexOf(i)
//      val inThat = thatIndexMap.indexOf(i)
//
//      (inThis >= 0, inThat >= 0) match {
//        case (true, false) => newIndices.append(thisIndices(inThis))
//        case (false, true) => newIndices.append(thatIndices(inThat))
//        case (true, true) =>
//          if (thisIndices(inThis) == thatIndices(inThat))
//            newIndices.append(thisIndices(inThis))
//          else
//            good = false
//        case _ => good = false
//      }
//    }
//
//    if (good)
//      Some(newIndices.toList)
//    else
//      None
//  }
}