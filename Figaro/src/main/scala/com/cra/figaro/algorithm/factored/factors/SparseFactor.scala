/*
 * SparseFactor.scala
 * Sparse implementation of factors over values.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
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
 * Sparse implementation of BasicFactor backed by a Map storage
 *
 * Factors are parameterized by the type of the Variables they contain and contain a semiring
 * that defines the mathematical operation to be performed on the values
 * Parent variables are distinguished from the output variable.
 *
 * This implementation stores only non-default (non-zero) elements and supplies special sum and product methods to
 * account for the missing values.
 */
class SparseFactor[T](val parents: List[Variable[_]], val output: List[Variable[_]], val semiring: Semiring[T] = SumProductSemiring().asInstanceOf[Semiring[T]])
    extends BasicFactor[T] {

  override def createFactor[T](_parents: List[Variable[_]], _output: List[Variable[_]], _semiring: Semiring[T] = semiring) = {
    val nf = new SparseFactor[T](_parents, _output, _semiring)
    nf
  }

  val contents: Map[List[Int], T] = Map()

  def getContents(): Traversable[T] = contents.values

  def stringContents(): String = contents.mkString(",")

  def contentsContains(index: List[Int]): Boolean = contents.contains(index)

  def set(indices: List[Int], value: T): Factor[T] = {
    contents += indices -> value
    this
  }

  /**
   * Get the value associated with a row. The row is identified by an list of indices
   * into the ranges of the variables over which the factor is defined. Rows with
   * default values will be missing, so supply the missing value for these rows
   */
  override def get(indices: List[Int]): T = {
    contents.get(indices) match {
      case Some(value) => value.asInstanceOf[T]
      case _           => semiring.zero
    }
  }

  /**
   * List the indices with non-default values
   */
  override def getIndices: Indices = new SparseIndices(variables)

  /**
   * Creates a new SparseFactor by applying the supplied function to all values of this factor.
   *
   * The new factor can have a new type parameter as well as a new semiring
   *
   * @param fn The function to apply to each value of this Factor
   * @param semiring A semiring to be used in the new Factor
   */
  override def mapTo[U](fn: T => U, _semiring: Semiring[U] = semiring): Factor[U] = {
    val newFactor = new SparseFactor[U](parents, output, _semiring)
    for { (key, value) <- contents } {
      newFactor.set(key, fn(value))
    }
    newFactor
  }

  /**
   * produces a new Factor from the combined input variables of the input factors.
   *
   * The factor values are computed using the corresponding values from the input factors. This correspondence
   * is determined by the overlap between the variables of this factor and the multiplier factor. When
   * indices from this factor and indices from the multiplier match on the overlapping variables, a new set of
   * indices is constructed for the new factor based on the two input indices and a new value is computed by
   * applying the op to the two input values. The new factor is updated with the new indices and the new value.
   *
   * @param that The Factor to combine with this one
   * @param op The operation used to combine (multiply) factor values
   * @return The new Factor containing the combined values of the inputs
   */
  override def combination(
    that: Factor[T],
    op: (T, T) => T): Factor[T] = {

    val (allParents, allChildren, thisIndexMap, thatIndexMap) = unionVars(that)

    // Find and map the Variables that the two multiplier Factors have in common
    val commonMap = thisIndexMap intersect thatIndexMap
    val thisCommon = commonMap map (thisIndexMap.indexOf(_))
    val thatCommon = commonMap map (thatIndexMap.indexOf(_))

    val result = createFactor[T](allParents, allChildren)
    val numVars = result.numVars

    // In a single pass create a map from the values of the overlapping variables to 
    // the contents of the other multiplier
    // TODO see if selecting which one to map makes a difference (eg the bigger one)
    val thatCommonValues: Map[List[Int], List[(List[Int], T)]] = Map()
    val thatSize = that.numVars
    for (thatIndices <- that.getIndices) {
      val thatValue = that.get(thatIndices)
      if (thatSize != thatIndices.size) {
        println("Found bad entry: " + thatIndices)
      }
      val key = thatCommon map (thatIndices(_))
      val matches = thatCommonValues.getOrElse(key, List())
      thatCommonValues.put(key, matches :+ (thatIndices, thatValue))
    }

    // For each of the current contents, find the overlap key
    // from the map of the other contents and combine the current
    // item with all the other items with the same overlap and
    // update the new factor
    for ((thisIndices, thisValue) <- this.contents) {
      // Create a search key based on the overlapping Variables
      val key = thisCommon map (thisIndices(_))
      // Use the key to check the 'that' map
      thatCommonValues.get(key) match {
        case Some(matches) => {
          var newIndices = new Array[Int](numVars)
          // insert 'this' indices into the newIndices
          for (i <- 0 until thisIndices.size) {
            newIndices(thisIndexMap(i)) = thisIndices(i)
          }

          for ((thatIndices, thatValue) <- matches) {
            // insert 'that' indices into the newIndices
            for (i <- 0 until thatIndices.size) {
              newIndices(thatIndexMap(i)) = thatIndices(i)
            }

            // calculate the new value and update the new Factor
            val newValue = op(thisValue, thatValue)
            result.set(newIndices.toList, newValue)
          }
        }
        case _ =>
      }
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
      //      val rest = List.tabulate(numVars)(n => n).diff(indicesOfSummedVariable)
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
