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
import com.cra.figaro.language.Parameter

/**
 * Sparse implementation of Factor. A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. Factors are parameterized by the types of values they contain.
 */
class SparseFactor[T](_parents: List[Variable[_]], _output: List[Variable[_]], _semiring: Semiring[T] = SumProductSemiring().asInstanceOf[Semiring[T]])
  extends BasicFactor[T](_parents, _output, _semiring) {

  override def createFactor[T](_parents: List[Variable[_]], _output: List[Variable[_]], _semiring: Semiring[T] = semiring) = {
    val nf = new SparseFactor[T](_parents, _output, _semiring)
    nf
  }

  /**
   * Get the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined. Rows with
   * default values will be missing, so supply the missing value for these rows
   */
  override def get(indices: List[Int]): T = {
    contents.get(indices) match {
      case Some(value) => value.asInstanceOf[T]
      case _ => semiring.zero
    }
  }

  /**
   * List the indices with non-default values
   */
  override def getIndices: Indices = new SparseIndices(variables)

  /**
   * Convert the contents of the target by applying the given function to all elements of this factor.
   */
  override def mapTo[U](fn: T => U, _semiring: Semiring[U] = semiring): Factor[U] = {
    val newFactor = new SparseFactor[U](parents, output, _semiring)
    for { (key, value) <- contents } {
      newFactor.set(key, fn(value))
    }
    newFactor
  }

  /**
   * Generic combination function for factors. By default, this is product, but other operations
   * (such as divide that is a valid operation for some semirings) can use this
   */
  override def combination(
    that: Factor[T],
    op: (T, T) => T): Factor[T] = {


    val (allParents, allChildren, thisIndexMap, thatIndexMap) = unionVars(that)
    val commonMap = thisIndexMap intersect thatIndexMap
    val thisCommon = commonMap map (thisIndexMap.indexOf(_))
    val thatCommon = commonMap map (thatIndexMap.indexOf(_))

    val result = createFactor[T](allParents, allChildren)
    val numVars = result.numVars

    val thatCommonValues: Map[List[Int], List[(List[Int], T)]] = Map()
    val thatSize = that.numVars
    for ((thatIndices, thatValue) <- that.contents) {
      if (thatSize != thatIndices.size) {
        println("Found bad entry: " + thatIndices)
      }
      val key = thatCommon map (thatIndices(_))
      val matches = thatCommonValues.getOrElse(key, List())
      thatCommonValues.put(key, matches :+ (thatIndices, thatValue))
    }
    
    for ((thisIndices, thisValue) <- this.contents) {
      val key = thisCommon map (thisIndices(_))
      thatCommonValues.get(key) match {
        case Some(matches) => {
          var newIndices = new Array[Int](numVars)
          for (i <- 0 until thisIndices.size) {
            newIndices(thisIndexMap(i)) = thisIndices(i)
          }
          
          for ((thatIndices, thatValue) <- matches) {
            for (i <- 0 until thatIndices.size) {
              newIndices(thatIndexMap(i)) = thatIndices(i)
            }

            val newValue = op(thisValue, thatValue)
            result.set(newIndices.toList, newValue)
          }
        }
        case _ =>
      }
      //      Factor.combineIndices(thisIndices, thisIndexMap, thatIndices, thatIndexMap, numVars) match {
      //        case Some(newIndices) =>
      //          val value = op(get(thisIndices), that.get(thatIndices))
      //          result.set(newIndices.toList, value)
      //        case None =>
      //      }
    }
    result
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

  override def recordArgMax[U](variable: Variable[U], comparator: (T, T) => Boolean, _semiring: Semiring[U] = semiring.asInstanceOf[Semiring[U]]): Factor[U] = {
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

    val result = createFactor[U](newParents, newOutput, _semiring)

    for { indices <- newIndices } yield {
      result.set(indices, computeArgMax(indices, variable, indicesOfSummedVariable, indicesSummed, comparator))
    }
    result
  }

  class SparseIndices(variables: List[Variable[_]]) extends Indices(variables) {
    override def iterator = contents.keys.iterator

  }
}
