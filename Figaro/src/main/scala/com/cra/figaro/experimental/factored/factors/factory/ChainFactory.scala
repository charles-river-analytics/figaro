/*
 * ChainFactory.scala
 * Create a factor from a Chain element.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 22, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.experimental.factored.factors.factory

import com.cra.figaro.experimental.factored.factors.{BasicFactor, InternalVariable}
import com.cra.figaro.algorithm.PointMapper
import com.cra.figaro.algorithm.factored.factors.{Factor, SparseFactor, Variable}
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._
import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe
import com.cra.figaro.algorithm.factored.factors.ConditionalSelector
/**
 * @author Glenn Takata Mar 22, 2015
 *
 */
object ChainFactory {

  def makeFactors[T, U](chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainMap: scala.collection.mutable.Map[T, Element[U]] = LazyValues(chain.universe).getMap(chain)
    val outputVar = Variable(chain)
    val parentVar = Variable(chain.parent)
    val selectorVar = makeSelectorVariable(parentVar, outputVar)
    val selector = new SparseFactor[Double](List(parentVar, selectorVar), List(outputVar))

    var tempFactors = new ListBuffer[Factor[Double]]
    tempFactors.append(selector)
    for {
      (parent, parentIndex) <- parentVar.range.zipWithIndex
     } {
      // parentVal.value should have been placed in applyMap at the time the values of this apply were computed.
      // By using chainMap, we can make sure that the result element is the same now as they were when values were computed.
      if (parent.isRegular) {
          tempFactors.append(makeSelection(chain, selectorVar, parentIndex, Variable(chainMap(parent.value)), selector)(mapper))
      }
      else {
        // We create a dummy variable for the outcome variable whose value is always star.
        // We create a dummy factor for that variable.
        // Then we use makeSelection with the dummy variable
        val dummy = new Variable(ValueSet.withStar[U](Set()))
        val dummyFactor = new BasicFactor[Double](List(), List(dummy))
        dummyFactor.set(List(0), 1.0)
        tempFactors.append(makeSelection(chain, selectorVar, parentIndex, dummy, selector), dummyFactor)
      }
    }

    tempFactors.toList
  }

  private def makeSelectorVariable(parent: Variable[_], overallVar: Variable[_]): Variable[Int] = {
    val selectorSize = parent.size * overallVar.size

    val values = SortedSet[Int]((0 to selectorSize): _*)
    new InternalVariable(ValueSet.withoutStar(values))
  }
  /*
   * The conditional selector creates a factor in which, when the selector's value is such that the result
   * element is relevant to the final result, the result element and overall element must have the same
   * value (handled by makeCares). Otherwise, the result element and overall element can take on any
   * value (handled by makeDontCares)
   */
  /**
   * Make a selection factor used in the decomposition of chain.
   * A chain defines a factor over the parent element, each of the possible result elements of the chain,
   * and the overall chain element. This can produce a very large factor when there are many result elements.
   * This is solved by decomposing the chain factor into a product of factors, each of which contains the
   * parent element, one of the result elements, and the overall chain element.
   */
  private def makeSelection[U](outputElement: Element[U], selectorVar: Variable[_],
    parentIndex: Int, outcomeVar: Variable[U], selector: Factor[Double])(implicit mapper: PointMapper[U]): Factor[Double] = {
    val outputVar = Variable(outputElement)
    val outputValues = LazyValues(outputElement.universe).storedValues(outputElement)
    val factor = new ConditionalSelector[Double](List(selectorVar), List(outcomeVar))

    makeCares(factor, parentIndex, outcomeVar, outputVar, outputValues.regularValues, selector)(mapper)

    factor
  }

  private def makeCares[U](factor: SparseFactor[Double], parentIndex: Int, outcomeVar: Variable[U], 
      outputVar: Variable[U], choices: Set[U], selector: Factor[Double])(implicit mapper: PointMapper[U]): Unit = {
    // We care to match up overallVar with outcomeVar
    val outputSize = outputVar.size
    for {
      (outcomeVal, j) <- outcomeVar.range.zipWithIndex
      (outputVal, k) <- outputVar.range.zipWithIndex
    } {
      // Star stands for "something". If outcomeVal is Star and overallVal is Star, we know something will match something, so the entry is (1,1).
      // If outcomeVal is Star and overallVal is a regular value, then maybe there will be a match, so the entry is (0,1).
      // If outcomeVal is regular, all the probability mass associated with that outcome should be on regular values of overallVal, so the entry is (0,0).
      val entry =
        if (outputVal.isRegular && outcomeVal.isRegular) {
          if (outputVal.value == mapper.map(outcomeVal.value, choices)) 1.0
          else 0.0
        } else if (!outputVal.isRegular && !outcomeVal.isRegular) 1.0
        else 0.0
        
      val selectorIndex = parentIndex * outputSize + k
      selector.set(List(parentIndex, selectorIndex, k), 1.0)
      factor.set(List(selectorIndex, j), entry)
    }
  }
}