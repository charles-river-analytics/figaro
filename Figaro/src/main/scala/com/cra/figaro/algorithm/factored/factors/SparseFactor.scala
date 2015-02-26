/*
 * SparseFactor.scala
 * Sparse implementation of factors over values.
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

import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.control.Breaks._
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.lazyfactored.Extended
import scala.reflect.runtime.universe._

/**
 * Sparse implementation of Factor. A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. Factors are parameterized by the types of values they contain.
 */
class SparseFactor[T](parents: List[Variable[_]], output: List[Variable[_]])(implicit tag: TypeTag[T])
  extends BasicFactor[T](parents, output) {
  
  override def createFactor[T: TypeTag](parents: List[Variable[_]], output: List[Variable[_]]) =
    new SparseFactor[T](parents, output)
    
 /**
   * Get the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined. Rows with 
   * default values will be missing, so supply the missing value for these rows
   */
  override def get(indices: List[Int]): T = {
    contents.get(indices) match {
      case Some(value) => value.asInstanceOf[T]
      case _ => defaultValue.asInstanceOf[T]
    }
  }

  /**
   * Set the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined.
   */
  override def set(indices: List[Int], value: T): Factor[T] = {
    if (!(value == defaultValue))
      contents += indices -> value
    this
  }
  
  /**
   * List the indices with non-default values
   */
  override def getIndices = contents.keys.toList

  /**
   * Generic combination function for factors. By default, this is product, but other operations
   * (such as divide that is a valid operation for some semirings) can use this
   */
  override def combination(
    that: Factor[T],
    op: (T, T) => T,
    semiring: Semiring[T]): Factor[T] = {
    val (allParents, allChildren, thisIndexMap, thatIndexMap) = unionVars(that)
    val result = createFactor[T](allParents, allChildren)

    for {
      thisIndices <- getIndices
      thatIndices <- that.getIndices
    } {
      Factor.combineIndices(thisIndices, thisIndexMap, thatIndices, thatIndexMap, result.numVars) match {
        case Some(newIndices) =>
          val value = op(get(thisIndices), that.get(thatIndices))
          result.set(newIndices.toList, value)
        case None =>
      }
    }
    result
  }

  private def computeSum(
    resultIndices: List[Int],
    summedVariable: Variable[_],
    summedVariableIndices: List[Int],
    summedNonZeroIndices: List[Int],
    semiring: Semiring[T]): T = {
    var value = semiring.zero
    val values =
      for { i <- summedNonZeroIndices } yield {
        val sourceIndices = insertAtIndices(resultIndices, summedVariableIndices, i)
        if (contents.contains(sourceIndices)) get(sourceIndices) else semiring.zero
      }
    semiring.sumMany(values)
  }

  override def sumOver(
    variable: Variable[_],
    semiring: Semiring[T]): SparseFactor[T] = {
    if (variables contains variable) {
      // The summed over variable does not necessarily appear exactly once in the factor.
      val indicesOfSummedVariable = indices(variables, variable)

      val newParents = parents.filterNot(_ == variable)
      val newOutput = output.filterNot(_ == variable)

      val result = createFactor[T](newParents, newOutput)

      // Compute the indices of the remaining variables 
      val newIndices = this.contents.keys.map(index => {
        val rest = List.tabulate(numVars)(n => n).diff(indicesOfSummedVariable)
        rest.map(i => index(i))
      })

      // Compute the indices of the summed out variable
      val indicesSummed = this.contents.keys.map(index => {
        val rest = List.tabulate(numVars)(n => n).diff(indicesOfSummedVariable)
        indicesOfSummedVariable.map(i => index(i))
      }).toList.flatten.distinct

      for { indices <- newIndices } {
        result.set(indices, computeSum(indices, variable, indicesOfSummedVariable, indicesSummed, semiring))
      }
      result
    } else this
  }

  private def computeArgMax[U](
    resultIndices: List[Int],
    summedVariable: Variable[U],
    summedVariableIndices: List[Int],
    summedNonZeroIndices: List[Int],
    comparator: (T, T) => Boolean): U = {
    val valuesWithEntries =
      for {
        //i <- 0 until summedVariable.size
        i <- summedNonZeroIndices
        xvalue = summedVariable.range(i)
        index = insertAtIndices(resultIndices, summedVariableIndices, i)
        if (xvalue.isRegular && contains(index))
      } yield (summedVariable.range(i).value, get(index))
    def process(best: (U, T), next: (U, T)) =
      if (comparator(best._2, next._2)) next; else best
    if (valuesWithEntries.isEmpty) {
      // Will this crash if there are no regular values?
      summedVariable.range.find(_.isRegular).get.value
    } else {
      valuesWithEntries.reduceLeft(process(_, _))._1
    }
  }

  override def recordArgMax[U: TypeTag](variable: Variable[U], comparator: (T, T) => Boolean): Factor[U] = {
    if (!(variables contains variable)) throw new IllegalArgumentException("Recording value of a variable not present")
    val indicesOfSummedVariable = indices(variables, variable)

    val newParents = parents.filterNot(_ == variable)
    val newOutput = output.filterNot(_ == variable)

    // Compute the indices of the summed out variable
    val indicesSummed = this.contents.keys.map(index => {
      val rest = List.tabulate(numVars)(n => n).diff(indicesOfSummedVariable)
      indicesOfSummedVariable.map(i => index(i))
    }).toList.flatten.distinct

    // Compute the indices of the remaining variables 
    val newIndices = this.contents.keys.map(index => {
      val rest = List.tabulate(numVars)(n => n).diff(indicesOfSummedVariable)
      rest.map(i => index(i))
    })

    val result = createFactor[U](newParents, newOutput)

    for { indices <- newIndices } yield {
      result.set(indices, computeArgMax(indices, variable, indicesOfSummedVariable, indicesSummed, comparator))
    }
    result
  }

}
