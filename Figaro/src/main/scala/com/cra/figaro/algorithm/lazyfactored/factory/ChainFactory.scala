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
package com.cra.figaro.algorithm.lazyfactored.factory

import scala.collection.mutable.ListBuffer
import com.cra.figaro.algorithm.PointMapper
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._
import com.cra.figaro.util._

/**
 * @author Glenn Takata Mar 22, 2015
 *
 */
object ChainFactory {

  /**
   * Factor constructor for a Chain Element
   */
  def makeFactors[T, U](chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainMap: scala.collection.mutable.Map[T, Element[U]] = LazyValues(chain.universe).getMap(chain)
    val outputVar = Variable(chain).asInstanceOf[ElementVariable[U]]
    val parentVar = Variable(chain.parent)

    // create a selectionVar and Factor to take the place of the parent/output combination
    val selectorVar = makeSelectorVariable(parentVar, outputVar)
    val selector = new SparseFactor[Double](List(parentVar, selectorVar), List(outputVar))
    val parentSize = parentVar.range.size

    var tempFactors = new ListBuffer[Factor[Double]]
    tempFactors.append(selector)
    // selector must be passed to makeSelection as it is updated based on the resultVar/chainVar match
    for {
      (parent, parentIndex) <- parentVar.range.zipWithIndex
    } {
      // parentVal.value should have been placed in applyMap at the time the values of this apply were computed.
      // By using chainMap, we can make sure that the result element is the same now as they were when values were computed.
      if (parent.isRegular) {
        tempFactors.append(makeSelection(chain, selectorVar, parentSize, parentIndex, Variable(chainMap(parent.value)), selector)(mapper))
      } else {
        // We create a dummy variable for the outcome variable whose value is always star.
        // We create a dummy factor for that variable.
        // Then we use makeSelection with the dummy variable
        val dummy = new Variable(ValueSet.withStar[U](Set()))
        val dummyFactor = new BasicFactor[Double](List(), List(dummy))
        dummyFactor.set(List(0), 1.0)
        tempFactors.append(makeSelection(chain, selectorVar, parentSize, parentIndex, dummy, selector), dummyFactor)
      }
    }

    tempFactors.toList
  }

  /*
   * Create a temporary variable representing the combination of the parent variable and the chain
   * variable
   */
  private def makeSelectorVariable[U](parent: Variable[_], overallVar: ElementVariable[U]): Variable[_] = {
    val selectorSize = parent.size * overallVar.size

    val values: List[List[Extended[_]]] = parent.range.flatMap(p => overallVar.range.map(o => List(p, o)))
    
    val tupleRangeRegular: List[List[_]] = cartesianProduct(List(parent, overallVar).map(_.range): _*)
    val tupleVS: ValueSet[List[Extended[_]]] = ValueSet.withoutStar(tupleRangeRegular.map(_.asInstanceOf[List[Extended[_]]]).toSet)
    
    new InternalChainVariable(tupleVS, overallVar.element.asInstanceOf[Chain[_,U]], overallVar.asInstanceOf[Variable[U]])
  }

  /**
   * Make a selection factor used in the decomposition of chain.
   * A chain defines a factor over the parent element, each of the possible result elements of the chain,
   * and the overall chain element. This can produce a very large factor when there are many result elements.
   * This is solved by decomposing the chain factor into a product of factors, each of which contains a
   * selector variable and the result variable.
   *
   * The conditional selector creates a factor in which, when the selector's value is such that the result
   * element is relevant to the final result, the result element and chain element must have the same
   * value (handled by makeCares). Otherwise, the result element and chain element can take on any
   * value (handled by makeDontCares)
   *
   */
  private def makeSelection[U](chain: Element[U], selectorVar: Variable[_], parentSize: Int,
    parentIndex: Int, resultVar: Variable[U], selector: Factor[Double])(implicit mapper: PointMapper[U]): Factor[Double] = {
    val chainVar = Variable(chain)
    val chainValues = LazyValues(chain.universe).storedValues(chain)
    val factor = new ConditionalSelector[Double](List(selectorVar), List(resultVar))

    makeCares(factor, parentIndex, resultVar, chainVar, chainValues.regularValues, selector)(mapper)
    for (index <- 0 until parentSize) {
      if (index != parentIndex)
        makeDontCares[U](factor, index, resultVar, chainVar, selector)
    }
    factor
  }

  /*
   * Values that specify the potential for the selector variable and the specified output
   */
  private def makeCares[U](factor: ConditionalSelector[Double], parentIndex: Int, resultVar: Variable[U],
    chainVar: Variable[U], choices: Set[U], selector: Factor[Double])(implicit mapper: PointMapper[U]): Unit = {
    // We care to match up overallVar with outcomeVar
    val chainSize = chainVar.size
    for {
      (resultVal, j) <- resultVar.range.zipWithIndex
      (chainVal, k) <- chainVar.range.zipWithIndex
    } {
      // Star stands for "something". If outcomeVal is Star and overallVal is Star, we know something will match something, so the entry is (1,1).
      // If outcomeVal is Star and overallVal is a regular value, then maybe there will be a match, so the entry is (0,1).
      // If outcomeVal is regular, all the probability mass associated with that outcome should be on regular values of overallVal, so the entry is (0,0).
      val entry =
        if (chainVal.isRegular && resultVal.isRegular) {
          if (chainVal.value == mapper.map(resultVal.value, choices)) 1.0
          else 0.0
        } else if (!chainVal.isRegular && !resultVal.isRegular) 1.0
        else 0.0

      // calculate the relevant index of the selector Factor and update both
      // the selector Factor and the current (conditional selector) Factor
      val selectorIndex = parentIndex * chainSize + k
      selector.set(List(parentIndex, selectorIndex, k), 1.0)
      factor.set(List(selectorIndex, j), entry)
    }
  }

  /*
   * Values that are irrelevant to the selector variable and specified output
   */
  private def makeDontCares[U](factor: ConditionalSelector[Double],
    parentIndex: Int,
    outcomeVar: Variable[U],
    chainVar: Variable[U], selector: Factor[Double]): Unit = {

    val chainSize = chainVar.size

    // If we don't care, we assign 1.0 to all combinations of the distVar and outcomeVar
    for {
      j <- 0 until outcomeVar.size
      k <- 0 until chainSize
    } {

      // calculate the relevant index of the selector Factor and update
      // current (conditional selector) Factor
      // The selector factor is sparse and needs no update
      val selectorIndex = parentIndex * chainSize + k
      factor.set(List(selectorIndex, j), 1.0)
    }
  }
}