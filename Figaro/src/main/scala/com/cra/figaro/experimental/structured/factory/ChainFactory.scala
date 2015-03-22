/*
 * Importance.scala
 * Importance sampler.
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
  def makeFactors[T, U](cc: ComponentCollection, chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
//    val chainMap: scala.collection.mutable.Map[T, Element[U]] = LazyValues(chain.universe).getMap(chain)
    val parentVar = Factory.getVariable(cc, chain.parent)
    val chainVar = Factory.getVariable(cc, chain)
    var tempFactors = parentVar.range.zipWithIndex flatMap (pair => {
      val parentVal = pair._1
      // parentVal.value should have been placed in applyMap at the time the values of this apply were computed.
      // By using chainMap, we can make sure that the result element is the same now as they were when values were computed.
//      if (parentVal.isRegular) List(Factory.makeConditionalSelector(cc, chain, parentVar, pair._2, Variable(chainMap(parentVal.value)))(mapper))
      if (parentVal.isRegular) {
        val outcomeElem = cc.expansions((chain.chainFunction, parentVal.value)).target.asInstanceOf[Element[U]]
        List(Factory.makeConditionalSelector(cc, chainVar, parentVar, pair._2, Factory.getVariable(cc, outcomeElem)))
      }
      else {
        // We create a dummy variable for the outcome variable whose value is always star.
        // We create a dummy factor for that variable.
        // Then we use makeConditionalSelector with the dummy variable
        val dummy = new Variable(ValueSet.withStar[U](Set()))
//        val dummyFactor = new BasicFactor[Double](List(), List(dummy))
//        dummyFactor.set(List(0), 1.0)
//        List(Factory.makeConditionalSelector(cc, chain, parentVar, pair._2, dummy), dummyFactor)
        List(Factory.makeConditionalSelector(cc, chainVar, parentVar, pair._2, dummy))
      }
    })
    tempFactors
  }

}
