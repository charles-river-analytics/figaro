/*
 * SelectFactory.scala
 * Methods to create factors for Select and Dist elements.
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

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.library.compound._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.algorithm.lazyfactored.Star

/**
 * A Sub-Factory for Select or Dist Elements
 */
object SelectFactory {

  /**
   * Factor constructor for an AtomicDistFlip
   */
  def makeFactors[T](cc: ComponentCollection, dist: AtomicDist[T]): List[Factor[Double]] = {
    val (intermed, clauseFactors) = intermedAndClauseFactors(cc, dist)
    val intermedFactor = makeSimpleDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  /**
   * Factor constructor for a CompoundDist
   */
  def makeFactors[T](cc: ComponentCollection, dist: CompoundDist[T]): List[Factor[Double]] = {
    val (intermed, clauseFactors) = intermedAndClauseFactors(cc, dist)
    val probVars = dist.probs map (Factory.getVariable(cc, _))
    val intermedFactor = makeComplexDistribution(cc, intermed, probVars)
    intermedFactor :: clauseFactors
  }

  /**
   * Factor constructor for an AtomicSelect
   */
  def makeFactors[T](cc: ComponentCollection, select: AtomicSelect[T]): List[Factor[Double]] = {
    val selectVar = Factory.getVariable(cc, select)
    if (selectVar.range.exists(!_.isRegular)) {
      assert(selectVar.range.size == 1) // Select's range must either be a list of regular values or {*}
      StarFactory.makeStarFactor(cc, select)
    } else {
      val probs = getProbs(cc, select)
      List(makeSimpleDistribution(selectVar, probs))
    }
  }

  /**
   * Factor constructor for a CompoundSelect
   */
  def makeFactors[T](cc: ComponentCollection, select: CompoundSelect[T]): List[Factor[Double]] = {
      val selectVar = Factory.getVariable(cc, select)
      val probs = getProbs(cc, select)
      val probVars = probs map (Factory.getVariable(cc, _))
      List(makeComplexDistribution(cc, selectVar, probVars))
  }

  /**
   * Factor constructor for a ParameterizedSelect
   */
  def makeFactors[T](cc: ComponentCollection, select: ParameterizedSelect[T], parameterized: Boolean): List[Factor[Double]] = {
    if (parameterized) {
      val selectVar = Factory.getVariable(cc, select)
      val probs = parameterizedGetProbs(cc, select)
      List(makeSimpleDistribution(selectVar, probs))
    } else {
      val selectVar: Variable[T] = Factory.getVariable(cc, select)
      if (selectVar.range.exists(!_.isRegular)) {
        // If the select has * in its range, the parameter must not have been added (because the parameter is an atomic beta)
        val factor = new BasicFactor[Double](List(), List(selectVar))
        for { (selectXval, i) <- selectVar.range.zipWithIndex } {
          val entry = if (selectXval.isRegular) 0.0 else 1.0
          factor.set(List(i), entry)
        }
        List(factor)
      } else {
        val paramVar: Variable[Array[Double]] = Factory.getVariable(cc, select.parameter)
        val factor = new BasicFactor[Double](List(paramVar), List(selectVar))
        for {
          (paramVal, paramIndex) <- paramVar.range.zipWithIndex
          (selectVal, selectIndex) <- selectVar.range.zipWithIndex
        } {
          val entry = paramVal.value(selectIndex)
          factor.set(List(paramIndex, selectIndex), entry)
        }
        List(factor)
      }
    }
  }

  /**
   * Factor constructor for an IntSelector
   */
  def makeFactors[T](cc: ComponentCollection, select: IntSelector): List[Factor[Double]] = {
    val elementVar = Factory.getVariable(cc, select)
    val counterVar = Factory.getVariable(cc, select.counter)
    val comb = new BasicFactor[Double](List(counterVar), List(elementVar))
    comb.fillByRule((l: List[Any]) => {
      val counterValue :: elementValue :: _ = l.asInstanceOf[List[Extended[Int]]]
      if (counterValue.isRegular && elementValue.isRegular) {
        if (elementValue.value < counterValue.value) 1.0 / counterValue.value; else 0.0
      } else 1.0

    })
    List(comb)
  }

  private def getProbs[U, T](cc: ComponentCollection, select: Select[U, T]): List[U] = getProbs(cc, select, select.clauses)

  /**
   * Get the potential (probability) for each value of an element, based on supplied rules
   */
  def getProbs[U, T](cc: ComponentCollection, elem: Element[T], clauses: List[(U, T)]): List[U] = {
    val selectVar = Factory.getVariable(cc, elem)
    def getProb(xvalue: Extended[T]): U = {
      clauses.find(_._2 == xvalue.value).get._1 // * cannot be a value of a Select
    }
    val probs =
      for { xvalue <- selectVar.range.filter(_.isRegular) } yield getProb(xvalue)
    probs
  }

  private def parameterizedGetProbs[T](cc: ComponentCollection, select: ParameterizedSelect[T]): List[Double] = {
    val outcomes = select.outcomes
    val map = select.parameter.MAPValue
    for {
      xvalue <- Factory.getVariable(cc, select).range
    } yield {
      if (xvalue.isRegular) map(outcomes.indexOf(xvalue.value)) else 0.0
    }
  }

  private def intermedAndClauseFactors[U, T](cc: ComponentCollection, dist: Dist[U, T]): (Variable[Int], List[Factor[Double]]) = {
    val intermed = Factory.makeVariable(cc, ValueSet.withoutStar((0 until dist.clauses.size).toSet))
    val distVar = Factory.getVariable(cc, dist)
    val (pairVar, pairFactor) = Factory.makeTupleVarAndFactor(cc, None, intermed, distVar)
    val clauseFactors = dist.outcomes.zipWithIndex map (pair =>
      Factory.makeConditionalSelector(pairVar, Regular(pair._2), Factory.getVariable(cc, pair._1), Set()))
    (intermed, pairFactor :: clauseFactors)
  }

  /**
   * Constructs a BasicFactor from a probability distribution. It assumes that the probabilities
   * are assigned to the Variable in the same order as it's values.
   */
  def makeSimpleDistribution[T](target: Variable[T], probs: List[Double]): Factor[Double] = {
    val factor = new BasicFactor[Double](List(), List(target))
    for { (prob, index) <- probs.zipWithIndex } {
      factor.set(List(index), prob)
    }
    factor
  }

  /*
   *  When one of the probability elements includes * in its range, so will the target element.
   *  This is necessary because when the value of any of the probability elements is *, the normalizing factor is unknown in this case,
   *  so we cannot assign a specific probability to any of the regular values. Instead, we assign probability 1 to *.
   *  The code in the method below is designed to take into account this case correctly.
   */
  private def makeComplexDistribution[T](cc: ComponentCollection, target: Variable[T], probVars: List[Variable[Double]]): Factor[Double] = {
    val nVars = probVars.size
    val factor = new BasicFactor[Double](probVars, List(target))
    val probVals: List[List[Extended[Double]]] = probVars map (_.range)
    if (target.range.forall(_.isRegular)) {
      /*
       * This is the easy case. For each list of indices to the factor, the first nVars indices will be indices into the range of the
       * probability variables, while the last index will be the index into the range of the target variable.
       * The correct probability is the value of the probability element in the given position with the appropriate index.
       * Because the probabilities may vary, we need to normalize them before putting in the factor.
       *
       * Note that the variables in the factor are ordered with the probability variables first and the target variable last.
       * But in probVals, the first index is the position into the target variable, and only then do we have the probability indices.
       */
      for { indices <- factor.getIndices } {
        val unnormalized =
          for { (probIndex, position) <- indices.toList.take(nVars).zipWithIndex } yield {
            val xprob = probVals(position)(probIndex) // The probability of the particular value of the probability element in this position
            xprob.value
          }
        val normalized = normalize(unnormalized).toArray
        factor.set(indices, normalized(indices.last))
      }
    } else {
      /*
       * In this case, the range of the target includes *. We do not assume any particular location in the range,
       * so we set targetStarIndex to the correct location.
       * Now, the indices to the factor will have nVar entries for each of the probability variables, plus one for the target.
       * When we get the entries for the probability variables, we get nVars. But the range of the target is nVars plus one,
       * because it includes *. So we extend the indices to probPlusStarIndices, with the targetStarIndex spliced in the correct place.
       *
       * Next, we distinguish between two cases. In the first case, one of the probability variables is *, so we assign probability 1
       * to the target being * and probability 0 everywhere. In the other case, all probability variables are regular, so we
       * assign the appropriate probability to the each regular position. We are careful to get the value of the
       * probability variable from its original position in the indices, and make sure we don't get the value of *.
       */
      val targetStarIndex: Int = target.range.indexWhere(!_.isRegular)
      for { indices <- factor.getIndices } {
        val probIndices: List[Int] = indices.toList.take(nVars)
        val probPlusStarIndices: List[Int] =
          probIndices.slice(0, targetStarIndex) ::: List(indices(targetStarIndex)) ::: probIndices.slice(targetStarIndex, probIndices.length)
        val allProbsRegularFlags: List[Boolean] =
          for { (probPlusStarIndex, position) <- probPlusStarIndices.zipWithIndex } yield {
            if (position < targetStarIndex) probVals(position)(probPlusStarIndex).isRegular
            else if (position == targetStarIndex) true
            else probVals(position)(probPlusStarIndex - 1).isRegular
          }
        val allProbsRegular: Boolean = allProbsRegularFlags.forall((b: Boolean) => b)

        val unnormalized: List[Double] =
          for { (probPlusStarIndex, position) <- probPlusStarIndices.zipWithIndex } yield {
          val xprob: Extended[Double] =
            if (position < targetStarIndex) probVals(position)(probPlusStarIndex)
            else if (position == targetStarIndex) Star[Double]
            else probVals(position)(probPlusStarIndex - 1)
          if (allProbsRegular) {
            if (position != targetStarIndex) xprob.value else 0.0
          } else {
            if (position == targetStarIndex) 1.0 else 0.0
          }
        }

        val normalized: Array[Double] = normalize(unnormalized).toArray
        // The first variable specifies the position of the remaining variables, so indices(0) is the correct probability
        factor.set(indices, normalized(indices.last))
      }
    }

    factor
  }
}
