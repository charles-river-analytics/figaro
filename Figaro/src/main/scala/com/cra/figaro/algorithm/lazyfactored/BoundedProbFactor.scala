/*
 * BoundedProbFactor.scala
 * Factors over variables.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 15, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.lazyfactored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import scala.language.existentials
import com.cra.figaro.algorithm.factored._

/**
 * Methods for creating lower and upper bound probability factors.
 */
object BoundedProbFactor {
  private def makeConditionAndConstraintFactors[T](elem: Element[T], upper: Boolean): List[Factor[Double]] =
    elem.allConditions.map(makeConditionFactor(elem, _, upper)) ::: elem.allConstraints.map(makeConstraintFactor(elem, _, upper))

  private def makeConditionFactor[T](elem: Element[T], cc: (T => Boolean, Element.Contingency), upper: Boolean): Factor[Double] =
    makeConstraintFactor(elem, (ProbConstraintType((t: T) => if (cc._1(t)) 1.0; else 0.0), cc._2), upper)

  private def makeConstraintFactor[T](elem: Element[T], cc: (T => Double, Element.Contingency), upper: Boolean): Factor[Double] = {
    val (constraint, contingency) = cc
    contingency match {
      case List() => makeUncontingentConstraintFactor(elem, constraint, upper)
      case first :: rest => makeContingentConstraintFactor(elem, constraint, first, rest, upper)
    }
  }

  private def makeUncontingentConstraintFactor[T](elem: Element[T], constraint: T => Double, upper: Boolean): Factor[Double] = {
    val elemVar = Variable(elem)
    val factor = Factory.make[Double](List(elemVar))
    for { (elemVal, index) <- elemVar.range.zipWithIndex } {
      val entry = if (elemVal.isRegular) {
        val c = math.exp(constraint(elemVal.value))
        c
      } else {
        // The (0,1) Double assume the constraint is always bounded between 0 and 1. This is always correct for conditions.
        // For constraints that could be greater than 1, the resulting Double on the answer could be wrong.
        if (upper) 1.0; else 0.0
      }
      factor.set(List(index), entry)
    }
    factor
  }

  private def makeContingentConstraintFactor[T](elem: Element[T], constraint: T => Double, firstConting: Element.ElemVal[_], restContinges: Element.Contingency, upper: Boolean): Factor[Double] = {
    val restFactor = makeConstraintFactor(elem, (constraint, restContinges), upper)
    extendConstraintFactor(restFactor, firstConting)
  }

  private def extendConstraintFactor(restFactor: Factor[Double], firstConting: Element.ElemVal[_]): Factor[Double] = {
    // The extended factor is obtained by getting the underlying factor and expanding each row so that the row only provides its entry if the contingent variable takes
    // on the appropriate value, otherwise the entry is 1
    val Element.ElemVal(firstElem, firstValue) = firstConting
    val firstVar = Variable(firstElem)
    val firstValues = firstVar.range
    val numFirstValues = firstValues.size
    val matchingIndex: Int = firstValues.indexOf(Regular(firstValue))
    val resultFactor = Factory.make[Double](firstVar :: restFactor.variables)
    for { restIndices <- restFactor.allIndices } {
      val restEntry = restFactor.get(restIndices)
      for { firstIndex <- 0 until numFirstValues } {
        // constraint doesn't apply if the index is not the required one, so we use a value of 1
        val resultEntry = if (firstIndex == matchingIndex) restEntry; else 1.0
        resultFactor.set(firstIndex :: restIndices, resultEntry)
      }
    }
    resultFactor
  }

  /**
   * Create the probabilistic factors associated with an element. 
   */
  def make(elem: Element[_], upper: Boolean): List[Factor[Double]] = {
    val constraintFactors = makeConditionAndConstraintFactors(elem, upper)
    constraintFactors ::: Factory.makeNonConstraintFactors(elem)
  }

  /**
   * Create the probabilistic factor encoding the probability of evidence in the dependent universe as a function of the
   * values of variables in the parent universe. The third argument is the the function to use for computing
   * probability of evidence in the dependent universe. It is assumed that the definition of this function will already contain the
   * right evidence.
   */
  def makeDependentFactor(parentUniverse: Universe,
    dependentUniverse: Universe,
    probEvidenceComputer: () => Double): (Factor[Double], Factor[Double]) = {
    val uses = dependentUniverse.parentElements filter (_.universe == parentUniverse)
    def rule(upper: Boolean)(values: List[Extended[_]]) = {
      if (values.exists(!_.isRegular)) { if (upper) 1.0; else 0.0 }
      else {
        for { (elem, value) <- uses zip values } { elem.value = value.asInstanceOf[Regular[elem.Value]].value }
        val result = probEvidenceComputer()
        result
      }
    }
    val variables = uses map (Variable(_))
    val lb = Factory.make[Double](variables)
    lb.fillByRule(rule(false))
    val ub = Factory.make[Double](variables)
    ub.fillByRule(rule(true))
    (lb, ub)
  }
}
