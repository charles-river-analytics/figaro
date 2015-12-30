/*
 * ConstraintFactory.scala
 * Methods to create factors for conditions and constraints on elements.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   March 1, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.Regular

object ConstraintFactory {
  /**
   * Make the constraint factors associated with the given element.
   * @param upper If true, upper bound factors will be created, otherwise lower bound
   */
  def makeFactors[T](cc: ComponentCollection, elem: Element[T], upper: Boolean): List[Factor[Double]] =
    elem.allConditions.map(makeConditionFactor(cc, elem, _, upper)) ::: elem.allConstraints.map(makeConstraintFactor(cc, elem, _, upper))

  private def makeConditionFactor[T](cc: ComponentCollection, elem: Element[T], contingentCondition: (T => Boolean, Element.Contingency), upper: Boolean): Factor[Double] =
    makeConstraintFactor(cc, elem, (ProbConstraintType((t: T) => if (contingentCondition._1(t)) 1.0; else 0.0), contingentCondition._2), upper)

  private def makeConstraintFactor[T](cc: ComponentCollection, elem: Element[T], contingentConstraint: (T => Double, Element.Contingency), upper: Boolean): Factor[Double] = {
    val (constraint, contingency) = contingentConstraint
    contingency match {
      case List() => makeUncontingentConstraintFactor(cc, elem, constraint, upper)
      case first :: rest => makeContingentConstraintFactor(cc, elem, constraint, first, rest, upper)
    }
  }

  private def makeUncontingentConstraintFactor[T](cc: ComponentCollection, elem: Element[T], constraint: T => Double, upper: Boolean): Factor[Double] = {
    val elemVar = Factory.getVariable(cc, elem)
    val factor = new ConstraintFactor[Double](List(), List(elemVar))
    for { (elemVal, index) <- elemVar.range.zipWithIndex } {
      val entry =
        if (elemVal.isRegular) {
          math.exp(constraint(elemVal.value))
        } else if (upper) {
          1.0
        } else {
          0.0
        }
      factor.set(List(index), entry)
    }
    factor
  }

  private def makeContingentConstraintFactor[T](cc: ComponentCollection, elem: Element[T], constraint: T => Double,
      firstConting: Element.ElemVal[_], restContinges: Element.Contingency, upper: Boolean): Factor[Double] = {
    val restFactor = makeConstraintFactor(cc, elem, (constraint, restContinges), upper)
    extendConstraintFactor(cc, restFactor, firstConting, upper)
  }

  private def extendConstraintFactor(cc: ComponentCollection, restFactor: Factor[Double], firstConting: Element.ElemVal[_], upper: Boolean): Factor[Double] = {
    // The extended factor is obtained by getting the underlying factor and expanding each row so that the row only provides its entry if the contingent variable takes
    // on the appropriate value, otherwise the entry is 1
    val Element.ElemVal(firstElem, firstValue) = firstConting
    val firstVar = Factory.getVariable(cc, firstElem)
    val firstValues = firstVar.range
    val numFirstValues = firstValues.size
    val matchingIndex: Int = firstValues.indexOf(Regular(firstValue))
    val resultFactor = new ConstraintFactor[Double](firstVar :: restFactor.parents, restFactor.output)
    for { restIndices <- restFactor.getIndices } {
      val restEntry = restFactor.get(restIndices)
      for { (firstValue, firstIndex) <- firstValues.zipWithIndex } {
        if (firstValue.isRegular) {
          val resultEntry = if (firstIndex == matchingIndex) restEntry; else 1.0
          resultFactor.set(firstIndex :: restIndices, resultEntry)
        } else {
          // Here's the logic for the lower and upper bounds:
          // The current firstValue is *. Let the value it will eventually resolve to be X.
          // If X is matching, the row's entry will be restEntry, otherwise 1.0.
          // So, the lower bound is restEntry while the upper is 1.0.
          val entry = if (upper) 1.0 else restEntry
          resultFactor.set(firstIndex :: restIndices, entry)
        }
      }
    }
    resultFactor
  }
}
