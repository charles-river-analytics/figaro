/*
 * DistributionFactory.scala
 * Description needed
 *
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Dec 15, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.structured.factory

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.{Regular, Star}
import com.cra.figaro.experimental.structured.ComponentCollection
//import com.cra.figaro.algorithm.lazyfactored._

/**
 * A Sub-Factory for simple probability distribution Elements
 */
object DistributionFactory {

  /**
   * Factor constructor for an AtomicFlip
   */
  def makeFactors(cc: ComponentCollection, flip: AtomicFlip): List[Factor[Double]] = {
    val flipVar = Factory.getVariable(cc, flip)
    if (flipVar.range.exists(!_.isRegular)) {
      assert(flipVar.range.size == 1) // Flip's range must either be {T,F} or {*}
      StarFactory.makeStarFactor(cc, flip)
    } else {
      val factor = new BasicFactor[Double](List(), List(flipVar))
      val i = flipVar.range.indexOf(Regular(true))
      factor.set(List(i), flip.prob)
      factor.set(List(1 - i), 1.0 - flip.prob)
      List(factor)
    }
  }

  /**
   * Factor constructor for a CompoundFlip
   */
  def makeFactors(cc: ComponentCollection, flip: CompoundFlip): List[Factor[Double]] = {
    val flipVar = Factory.getVariable(cc, flip)
    val probVar = Factory.getVariable(cc, flip.prob)
    val factor = new BasicFactor[Double](List(probVar), List(flipVar))
    val parentVals = probVar.range
    if (flipVar.range.exists(!_.isRegular)) {
      val falseIndex = flipVar.range.indexOf(Regular(false))
      val trueIndex = flipVar.range.indexOf(Regular(true))
      val starIndex = flipVar.range.indexWhere(!_.isRegular)
      for { j <- 0 until parentVals.size } {
        if (parentVals(j).isRegular) {
          val value = parentVals(j).value
          factor.set(List(j, trueIndex), value)
          factor.set(List(j, falseIndex), 1.0 - value)
          factor.set(List(j, starIndex), 0.0)
        } else {
          factor.set(List(j, trueIndex), 0.0)
          factor.set(List(j, falseIndex), 0.0)
          factor.set(List(j, starIndex), 1.0)
        }
      }
      List(factor)
    } else {
      val trueIndex = flipVar.range.indexOf(Regular(true))
      val falseIndex = 1 - trueIndex
      for { j <- 0 until parentVals.size } {
        val value = parentVals(j).value
        factor.set(List(j, trueIndex), value)
        factor.set(List(j, falseIndex), 1.0 - value)
      }
      List(factor)
    }
  }

  /**
   * Factor constructor for a ParameterizedFlip
   */
  def makeFactors(cc: ComponentCollection, flip: ParameterizedFlip): List[Factor[Double]] = {
    val flipVar = Factory.getVariable(cc, flip)
    val factor = new BasicFactor[Double](List(),List(flipVar))
    val prob = flip.parameter.MAPValue
    val i = flipVar.range.indexOf(Regular(true))
    factor.set(List(i), prob)
    factor.set(List(1 - i), 1.0 - prob)
    List(factor)
  }

  /**
   * Factor constructor for an AtomicBinomial
   */
  def makeFactors(cc: ComponentCollection, binomial: AtomicBinomial): List[Factor[Double]] = {
      val binVar = Factory.getVariable(cc, binomial)
      val factor = new BasicFactor[Double](List(), List(binVar))
      for { (xvalue, index) <- binVar.range.zipWithIndex } {
        factor.set(List(index), binomial.density(xvalue.value))
      }
    List(factor)
  }

}
