/*
 * BasicFactor.scala
 * Default implementation of factors over values.
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
import scala.collection.mutable.Map
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.lazyfactored.Extended

/**
 * Default implementation of Factor. A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. Factors are parameterized by the types of values they contain.
 */
class BasicFactor[T](val parents: List[Variable[_]], val output: List[Variable[_]]) extends Factor[T] {

  override def convert[U](): Factor[U] = {
    new BasicFactor[U](parents, output)
  }

  override def mapTo[U](fn: T => U): Factor[U] = {
    val newFactor = new BasicFactor[U](parents, output)
    for { (key, value) <- contents } {
      newFactor.set(key, fn(value))
    }
    newFactor
  }

  override def fillByRule(rule: List[Extended[_]] => T): Factor[T] = {
    val ranges: List[List[(Extended[_], Int)]] = variables map (_.range.zipWithIndex)
    val cases: List[List[(Extended[_], Int)]] = homogeneousCartesianProduct(ranges: _*)
    for { cas <- cases } {
      val (values, indices) = cas.unzip
      set(indices, rule(values))
    }
    this
  }

  // unionVars takes the variables in two factors and produces their union.
  private def unionVars[U](that: Factor[U]): (List[Variable[_]], List[Variable[_]], List[Int], List[Int]) = {
    val allParents = parents.union(that.parents).distinct
    val allOutputs = output.union(that.output).distinct diff (allParents)

    val resultVars = allParents ::: allOutputs
    val indexMap1 = variables map (resultVars.indexOf(_))
    val indexMap2 = that.variables map (resultVars.indexOf(_))
    (allParents, allOutputs, indexMap1, indexMap2)
  }

  override def product(
    that: Factor[T],
    semiring: Semiring[T]): Factor[T] = {
    combination(that, semiring.product)
  }

  override def combination(
    that: Factor[T],
    op: (T, T) => T): Factor[T] = {
    val (allParents, allChildren, indexMap1, indexMap2) = unionVars(that)
    val result = new BasicFactor[T](allParents, allChildren)

    //    val indexMap1 = variables map (result.variables.indexOf(_))
    //    val indexMap2 = that.variables map (result.variables.indexOf(_))

    for { indices <- result.allIndices } {
      val indexIntoThis = indexMap1 map (indices(_))
      val indexIntoThat = indexMap2 map (indices(_))
      val value = op(get(indexIntoThis), that.get(indexIntoThat))
      result.set(indices, value)
    }
    result
  }

  private def computeSum(
    resultIndices: List[Int],
    summedVariable: Variable[_],
    summedVariableIndices: List[Int],
    semiring: Semiring[T]): T = {
    var value = semiring.zero
    val values =
      for { i <- 0 until summedVariable.size } yield {
        val sourceIndices = insertAtIndices(resultIndices, summedVariableIndices, i)
        get(sourceIndices)
      }
    semiring.sumMany(values)
  }

  override def sumOver(
    variable: Variable[_],
    semiring: Semiring[T]): BasicFactor[T] = {
    if (variables contains variable) {
      // The summed over variable does not necessarily appear exactly once in the factor.
      val indicesOfSummedVariable = indices(variables, variable)

      val newParents = parents.filterNot(_ == variable)
      val newOutput = output.filterNot(_ == variable)

      val result = new BasicFactor[T](newParents, newOutput)
      for { indices <- result.allIndices } {
        result.set(indices, computeSum(indices, variable, indicesOfSummedVariable, semiring))
      }
      result
    } else this
  }

  private def computeArgMax[U](
    resultIndices: List[Int],
    summedVariable: Variable[U],
    summedVariableIndices: List[Int],
    comparator: (T, T) => Boolean): U = {
    def getEntry(i: Int) =
      get(insertAtIndices(resultIndices, summedVariableIndices, i))
    val valuesWithEntries =
      for {
        i <- 0 until summedVariable.size
        xvalue = summedVariable.range(i)
        if xvalue.isRegular
      } yield (summedVariable.range(i).value, getEntry(i))
    def process(best: (U, T), next: (U, T)) =
      if (comparator(best._2, next._2)) next; else best
    valuesWithEntries.reduceLeft(process(_, _))._1
  }

  override def recordArgMax[U](variable: Variable[U], comparator: (T, T) => Boolean): Factor[U] = {
    if (!(variables contains variable)) throw new IllegalArgumentException("Recording value of a variable not present")
    val indicesOfSummedVariable = indices(variables, variable)

    val newParents = parents.filterNot(_ == variable)
    val newOutput = output.filterNot(_ == variable)

    val result = new BasicFactor[U](newParents, newOutput)
    for { indices <- result.allIndices } yield {
      result.set(indices, computeArgMax(indices, variable, indicesOfSummedVariable, comparator))
    }
    result
  }

  override def marginalizeTo(
    semiring: Semiring[T],
    targets: Variable[_]*): Factor[T] = {
    val marginalized =
      (this /: variables)((factor: BasicFactor[T], variable: Variable[_]) =>
        if (targets contains variable) factor
        else factor.sumOver(variable, semiring))
    // It's possible that the target variable appears more than once in this factor. If so, we need to reduce it to
    // one column by eliminating any rows in which the target variable values do not agree.
    deDuplicate(marginalized)
  }

  override def deDuplicate(): Factor[T] =
    {
      deDuplicate(this)
    }

  private def deDuplicate(factor: Factor[T]): Factor[T] =
    {
      val repeats = findRepeats(factor.variables)
      val hasRepeats = (false /: repeats.values)(_ || _.size > 1)
      if (hasRepeats) {
        val reducedVariables = repeats.keySet.toList
        val reducedParents = reducedVariables.intersect(parents)
        val reducedChildren = reducedVariables.diff(reducedParents)
        val reduced = new BasicFactor[T](reducedParents, reducedChildren)
        val newVariableLocations = repeats.values.map(_(0))

        val repeatedVariables = repeats.values.filter(_.size > 1)
        for (row <- factor.allIndices) {
          if (checkRow(row, repeatedVariables)) {
            var newRow = List[Int]()
            for (pos <- newVariableLocations) {
              newRow = newRow :+ row(pos)
            }
            reduced.set(newRow, factor.get(row))
          }
        }
        reduced
      } else {
        factor
      }
    }

  private def checkRow(row: List[Int], repeatedVariables: Iterable[List[Int]]): Boolean = {
    var ok = true

    for (repeats <- repeatedVariables) {
      val checkVal = row(repeats(0))
      for (pos <- repeats) {
        if (checkVal != row(pos)) {
          ok = false
        }
      }
    }
    ok
  }

  private def findRepeats(varList: List[Variable[_]]): Map[Variable[_], List[Int]] =
    {
      var repeats = Map[Variable[_], List[Int]]()

      for (variable <- varList) {
        if (!repeats.keySet.contains(variable)) {
          var indices = List[Int]()
          var start = varList.indexOf(variable)
          while (start > -1) {
            indices = indices :+ start
            start = varList.indexOf(variable, start + 1)
          }
          repeats = repeats + (variable -> indices)
        }
      }
      repeats
    }

  override def toReadableString: String = {
    val result = new StringBuffer
    // layout has one column for each of the variables followed by a column for the result
    val valueWidths =
      for { variable <- variables } yield {
        val valueLengths = variable.range.map(_.toString.length)
        val maxValueLength = valueLengths.foldLeft(4)(_ max _)
        (maxValueLength max variable.id.toString.length) + 2 // add 2 for spaces
      }
    val resultWidth = contents.values.map(_.toString.length).foldLeft(4)(_ max _) + 2
    def addBorderRow() {
      for { width <- valueWidths } { result.append("|" + "-" * width) }
      result.append("|" + "-" * resultWidth + "|\n") //   
    }
    def addCentered(string: String, width: Int) {
      val buffer = (width - string.length) / 2
      val bufferRemainder = (width - string.length) % 2
      result.append(" " * buffer + string + " " * (buffer + bufferRemainder))
    }
    addBorderRow()
    // Header row
    for { (variable, width) <- variables zip valueWidths } {
      result.append("|")
      addCentered(variable.id.toString, width)
    }
    result.append("|" + " " * resultWidth + "|\n")
    addBorderRow()
    // Data rows
    for { indices <- allIndices } {
      val values = for { (variable, index) <- variables zip indices } yield { variable.range(index) }
      for { (value, width) <- values zip valueWidths } {
        result.append("|")
        addCentered(value.toString, width)
      }
      result.append("|")
      addCentered(contents(indices).toString, resultWidth)
      result.append("|\n")
    }
    addBorderRow()
    result.toString
  }

}
