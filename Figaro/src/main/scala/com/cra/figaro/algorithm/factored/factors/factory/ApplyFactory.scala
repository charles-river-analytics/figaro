/*
 * ApplyFactory.scala
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

package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm.PointMapper
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.language._

/**
 * A Sub-Factory for Apply Elements
 */
object ApplyFactory {
  
  /**
   * Factor constructor for an Apply Element that has one input
   */
  def makeFactors[T, U](apply: Apply1[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[T, U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var), List(resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      // See logic in makeCares
      val entry =
        if (arg1Val.isRegular && resultVal.isRegular) {
        // arg1Val.value should have been placed in applyMap at the time the values of this apply were computed.
        // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
        if (resultVal.value == mapper.map(applyMap(arg1Val.value), applyValues.regularValues)) 1.0
          else 0.0
        } else if (!arg1Val.isRegular && !resultVal.isRegular) 1.0
        else if (!arg1Val.isRegular && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, resultIndex), entry)
    }
    List(factor)
  }

  /**
   * Factor constructor for an Apply Element that has two inputs
   */
  def makeFactors[T1, T2, U](apply: Apply2[T1, T2, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var), List(resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, resultIndex), entry)
    }
    List(factor)
  }

  /**
   * Factor constructor for an Apply Element that has three inputs
   */
  def makeFactors[T1, T2, T3, U](apply: Apply3[T1, T2, T3, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2, T3), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, arg3Var), List(resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && arg3Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value, arg3Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, resultIndex), entry)
    }
    List(factor)
  }

  /**
   * Factor constructor for an Apply Element that has four inputs
   */
  def makeFactors[T1, T2, T3, T4, U](apply: Apply4[T1, T2, T3, T4, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2, T3, T4), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, arg3Var, arg4Var), List(resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val arg4Indices = arg4Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      (arg4Val, arg4Index) <- arg4Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && arg3Val.isRegular && arg4Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value, arg3Val.value, arg4Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, resultIndex), entry)
    }
    List(factor)
  }

  /**
   * Factor constructor for an Apply Element that has five inputs
   */
  def makeFactors[T1, T2, T3, T4, T5, U](apply: Apply5[T1, T2, T3, T4, T5, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val applyMap: scala.collection.mutable.Map[(T1, T2, T3, T4, T5), U] = LazyValues(apply.universe).getMap(apply)
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val arg5Var = Variable(apply.arg5)
    val resultVar = Variable(apply)
    val applyValues = LazyValues(apply.universe).storedValues(apply)
    val factor = new BasicFactor[Double](List(arg1Var, arg2Var, arg3Var, arg4Var, arg5Var), List(resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val arg4Indices = arg4Var.range.zipWithIndex
    val arg5Indices = arg5Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      (arg4Val, arg4Index) <- arg4Indices
      (arg5Val, arg5Index) <- arg5Indices
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry =
        if (arg1Val.isRegular && arg2Val.isRegular && arg3Val.isRegular && arg4Val.isRegular && arg5Val.isRegular && resultVal.isRegular) {
          // The argument values should have been placed in applyMap at the time the values of this apply were computed.
          // By using applyMap, we can make sure that any contained elements in the result of the apply are the same now as they were when values were computed.
          if (resultVal.value == mapper.map(applyMap((arg1Val.value, arg2Val.value, arg3Val.value, arg4Val.value, arg5Val.value)), applyValues.regularValues)) 1.0
          else 0.0
        } else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular || !arg5Val.isRegular) && !resultVal.isRegular) 1.0
        else if ((!arg1Val.isRegular || !arg2Val.isRegular || !arg3Val.isRegular || !arg4Val.isRegular || !arg5Val.isRegular) && resultVal.isRegular) 0.0
        else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, arg5Index, resultIndex), entry)
    }
    List(factor)
  }

}