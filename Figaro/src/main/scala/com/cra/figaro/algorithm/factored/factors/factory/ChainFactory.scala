/*
 * ChainFactory.scala
 * Methods to create factors associated with Chain elements.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm.PointMapper
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.language._
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.util._
/**
 * @author Glenn Takata Feb 19, 2015
 *
 */
object ChainFactory {
  /**
   * Make the factors associated with a chain element.
   */

  import com.cra.figaro.algorithm.factored.factors.factory.Factory

  def makeFactors[T, U](cc: ComponentCollection, chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainComp = cc(chain)
    if (chainComp.allSubproblemsEliminatedCompletely && cc.useSingleChainFactor) {
      makeSingleFactor[T, U](cc, chain)(mapper)
    } else {
      makeMultipleFactors[T, U](cc, chain)(mapper)
    }
  }

  def makeMultipleFactors[T, U](cc: ComponentCollection, chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainComp = cc(chain)
    val parentVar = Factory.getVariable(cc, chain.parent)
    val chainVar = Factory.getVariable(cc, chain)
    val (pairVar, pairFactor) = Factory.makeTupleVarAndFactor(cc, Some(chain), parentVar, chainVar)
    cc.variableParents(pairVar) = Set(parentVar, chainVar)
    var tempFactors = parentVar.range.zipWithIndex flatMap (pair => {
      val (parentVal, parentIndex) = pair
      if (parentVal.isRegular) {
        // We need to create an actual variable to represent the outcome of the chain.
        // This will have the same range as the formal variable associated with the expansion.
        // The formal variable will be replaced with the actual variable in the solution.
        // There are two possibilities.
        // If the outcome element is defined inside the chain, it will actually be different in every expansion,
        // even though the formal variable is the same. But if the outcome element is defined outside the chain,
        // it is a global that will be the same every time, so the actual variable is the variable of this global.
        val nestedProblem = cc.expansions((chain.chainFunction, parentVal.value))
        val outcomeElem = nestedProblem.target.asInstanceOf[Element[U]]
        val formalVar = Factory.getVariable(cc, outcomeElem)
        val actualVar = if (!nestedProblem.global(formalVar)) Factory.makeVariable(cc, formalVar.valueSet) else formalVar
        cc.variableParents(chainVar) += actualVar
        chainComp.actualSubproblemVariables += parentVal.value -> actualVar
        List(Factory.makeConditionalSelector(pairVar, parentVal, actualVar, chainComp.range.regularValues)(mapper))
      } else {
        // We create a dummy variable for the outcome variable whose value is always star.
        // We create a dummy factor for that variable.
        // Then we use makeConditionalSelector with the dummy variable
        val dummy = Factory.makeVariable(cc, ValueSet.withStar[U](Set()))
        List(Factory.makeConditionalSelector(pairVar, parentVal, dummy, chainComp.range.regularValues))
      }
    })
    pairFactor :: tempFactors
  }

  def makeSingleFactor[T, U](cc: ComponentCollection, chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainComp = cc(chain)
    val parentVar = Factory.getVariable(cc, chain.parent)
    val childVar = Factory.getVariable(cc, chain)
    val factor = new BasicFactor[Double](List(parentVar), List(childVar))
    for { parentIndex <- 0 until parentVar.range.length } {
      val parentXV = parentVar.range(parentIndex)
      if (parentXV.isRegular && chainComp.subproblems.contains(parentXV.value) && !chainComp.subproblems(parentXV.value).solution.isEmpty) {
        val subproblem = chainComp.subproblems(parentXV.value)
        // Need to normalize subsolution in case there's any nested evidence
        val subsolution = subproblem.solution.reduce(_.product(_))
        //val sum = subsolution.foldLeft(subsolution.semiring.zero, subsolution.semiring.sum(_, _))
        val subVars = subsolution.variables
        if (subVars.length == 1) {
          val subVar = subVars(0)
          for { subVal <- subVar.range } {
            val childIndex = childVar.range.indexOf(subVal)
            val subIndex = subVar.range.indexOf(subVal)
            //val entry = subsolution.semiring.product(subsolution.get(List(subIndex)), 1.0 / sum)
            factor.set(List(parentIndex, childIndex), subsolution.get(List(subIndex)))
          }
        } else { // This should be a case where the subproblem is empty and the value is *
          val starIndex = childVar.range.indexWhere(!_.isRegular)
          factor.set(List(parentIndex, starIndex), factor.semiring.one)
        }

      } else {
        for { childIndex <- 0 until childVar.range.length } {
          val entry = if (childVar.range(childIndex).isRegular) factor.semiring.zero else factor.semiring.one
          factor.set(List(parentIndex, childIndex), entry)
        }
        val childIndex = childVar.range.indexWhere(!_.isRegular)
        factor.set(List(parentIndex, childIndex), factor.semiring.one)
      }
    }
    List(factor)

  }

}
