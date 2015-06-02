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
package com.cra.figaro.experimental.structured.factory

import com.cra.figaro.algorithm.PointMapper
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.ValueSet
import com.cra.figaro.language._
import com.cra.figaro.experimental.structured.ComponentCollection
/**
 * @author Glenn Takata Feb 19, 2015
 *
 */
object ChainFactory {
  /**
   * Make the factors associated with a chain element.
   */
  def makeFactors[T, U](cc: ComponentCollection, chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainComp = cc(chain)
    val parentVar = Factory.getVariable(cc, chain.parent)
    val chainVar = Factory.getVariable(cc, chain)
    val (pairVar, pairFactor) = Factory.makeTupleVarAndFactor(cc, parentVar, chainVar)
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
        chainComp.actualSubproblemVariables += parentVal.value -> actualVar
        List(Factory.makeConditionalSelector(pairVar, parentVal, actualVar))
      }
      else {
        // We create a dummy variable for the outcome variable whose value is always star.
        // We create a dummy factor for that variable.
        // Then we use makeConditionalSelector with the dummy variable
        val dummy = Factory.makeVariable(cc, ValueSet.withStar[U](Set()))
        List(Factory.makeConditionalSelector(pairVar, parentVal, dummy))
      }
    })
    pairFactor :: tempFactors
  }

}
