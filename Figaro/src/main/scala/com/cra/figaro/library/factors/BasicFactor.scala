/*
 * Factor.scala
 * General class of factors over values.
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

import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.Map
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.lazyfactored.Extended

/**
 * General class of factors. A factor is associated with a set of variables and specifies a value for every
 * combination of assignments to those variables. Factors are parameterized by the types of values they contain.
 */
class BasicFactor[T](val variables: List[Variable[_]])  extends Factor[T] {

  /**
   * Fill the contents of the target by applying the given function to all elements of this factor.
   */
  override def mapTo[U](fn: T => U, variables: List[Variable[_]]): Factor[U] = {
    val newFactor = Factory.make[U](variables)
    for { (key, value) <- contents } {
      newFactor.contents += key -> fn(value) }
    newFactor
  }

  /**
   * Fill the contents of this factor by applying a rule to every combination of values.
   */
  override def fillByRule(rule: List[Extended[_]] => T):Factor[T] = {
    val ranges: List[List[(Extended[_], Int)]] = variables map (_.range.zipWithIndex)
    val cases: List[List[(Extended[_], Int)]] = homogeneousCartesianProduct(ranges: _*)
    for { cas <- cases } {
      val (values, indices) = cas.unzip
      contents += indices -> rule(values)
    }
    this
  }

  // unionVars takes the variables in two factors and produces their union. In addition, it produces two lists that
  // map indexMap in each input factor into indexMap in the union.
  private def unionVars[U](that: Factor[U]): (List[Variable[_]], List[Int], List[Int]) = {
    val resultVars = variables ++ (that.variables filter ((v: Variable[_]) => !(variables contains v)))
    val indexMap1 = variables.zipWithIndex map (_._2)
    val indexMap2 = that.variables map (resultVars.indexOf(_))
    (resultVars, indexMap1, indexMap2)
  }

  /**
   * Returns the product of this factor with another factor according to a given multiplication function.
   * The product is associated with all variables in the two inputs, and the value associated with an assignment
   * is the product of the values in the two inputs.
   */
   override def product(
    that: Factor[T],
    semiring: Semiring[T]): Factor[T] = {
   combination(that, semiring.product)
  }

  /**
   * Generic combination function for factors. By default, this is product, but other operations 
   * (such as divide that is a valid operation for some semirings) can use this
   */
  override def combination(
    that: Factor[T],
    op: (T, T) => T): Factor[T] = {
    val (allVars, indexMap1, indexMap2) = unionVars(that)
    val result = new BasicFactor[T](allVars)
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

  /**
   * Returns the summation of the factor over a variable according to an addition function.
   * The result is associated with all the variables in the
   * input except for the summed over variable and the value for a set of assignments is the
   * sum of the values of the corresponding assignments in the input.
   */
  override def sumOver(
    variable: Variable[_],
    semiring: Semiring[T]): BasicFactor[T] = {
    if (variables contains variable) {
      // The summed over variable does not necessarily appear exactly once in the factor.
      val indicesOfSummedVariable = indices(variables, variable)
      val resultVars = variables.toList.filterNot(_ == variable)
      val result = new BasicFactor[T](resultVars)
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

  /**
   * Returns a factor that maps values of the other variables to the value of the given variable that
   * maximizes the entry associated with that value, according to some maximization function.
   * comparator defines the maximization. It returns true iff its second argument is greater than its first.
   *
   * @tparam U The type of element whose value is being recorded. The resulting factor maps values of
   * other variables in this factor to this type.
   * @tparam T The type of entries of this factor.
   */
  override def recordArgMax[U](variable: Variable[U], comparator: (T, T) => Boolean): Factor[U] = {
    if (!(variables contains variable)) throw new IllegalArgumentException("Recording value of a variable not present")
    val indicesOfSummedVariable = indices(variables, variable)
    val resultVars = variables.toList.filterNot(_ == variable)
    val result = new BasicFactor[U](resultVars)
    for { indices <- result.allIndices } yield {
      result.set(indices, computeArgMax(indices, variable, indicesOfSummedVariable, comparator))
    }
    result
  }

  /**
   * Returns the marginalization of the factor to a variable according to the given addition function.
   * This involves summing out all other variables.
   */
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
        val reduced = new BasicFactor[T](repeats.keySet.toList)
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

  /**
   * Produce a readable string representation of the factor
   */
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
