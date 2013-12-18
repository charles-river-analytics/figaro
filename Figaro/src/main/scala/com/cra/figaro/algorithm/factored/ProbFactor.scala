/*
 * ProbFactor.scala
 * Factors over variables.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import annotation.tailrec
import scala.language.existentials

/**
 * Methods for creating probabilistic factors associated with elements.
 */
object ProbFactor {
  private def makeFactors[T](const: Constant[T]): List[Factor[Double]] = {
    val factor = new Factor[Double](List(Variable(const)))
    factor.set(List(0), 1.0)
    List(factor)
  }

  private def makeFactors(flip: AtomicFlip): List[Factor[Double]] = {
    val flipVar = Variable(flip)
    val factor = new Factor[Double](List(flipVar))
    val i = flipVar.range.indexOf(true)
    factor.set(List(i), flip.prob)
    factor.set(List(1 - i), 1.0 - flip.prob)
    List(factor)
  }

  private def makeFactors(flip: CompoundFlip): List[Factor[Double]] = {
    val flipVar = Variable(flip)
    val probVar = Variable(flip.prob)
    val factor = new Factor[Double](List(probVar, flipVar))
    val parentVals = probVar.range
    val i = flipVar.range.indexOf(true)
    for { j <- 0 until parentVals.size } {
      factor.set(List(j, i), parentVals(j))
      factor.set(List(j, 1 - i), 1.0 - parentVals(j))
    }
    List(factor)
  }

  private def makeSimpleDistribution[T](target: Variable[T], probs: List[Double]): Factor[Double] = {
    val factor = new Factor[Double](List(target))
    for { (prob, index) <- probs.zipWithIndex } {
      factor.set(List(index), prob)
    }
    factor
  }

  private def makeComplexDistribution[T](target: Variable[T], probElems: List[Element[Double]]): Factor[Double] = {
    val probVars = probElems map (Variable(_))
    val factor = new Factor[Double](List((target :: probVars): _*))
    val probVals = probVars map (_.range)
    for { indices <- factor.allIndices } {
      val probIndices = indices.toList.tail.zipWithIndex
      val unnormalized = probIndices map (pair => probVals(pair._2)(pair._1))
      val normalized = normalize(unnormalized).toArray
      factor.set(indices, normalized(indices(0)))
    }
    factor
  }

  private def selectVarAndProbs[U, T](select: Select[U, T]): (Variable[T], List[U]) = {
    val selectVar = Variable(select)
    val probs = for { value <- selectVar.range } yield select.clauses.find(_._2 == value).get._1
    (selectVar, probs)
  }

  private def parameterizedSelectVarAndProbs[T](select: ParameterizedSelect[T]): (Variable[T], List[Double]) = {
    val selectVar = Variable(select)
    val probs = select.parameter.expectedValue.toList
    (selectVar, probs)
  }

  private def makeFactors[T](select: AtomicSelect[T]): List[Factor[Double]] = {
    val (selectVar, probs) = selectVarAndProbs(select)
    List(makeSimpleDistribution(selectVar, probs))
  }

  private def makeFactors[T](select: CompoundSelect[T]): List[Factor[Double]] = {
    val (selectVar, probs) = selectVarAndProbs(select)
    List(makeComplexDistribution(selectVar, probs))
  }

  private def makeFactors[T](select: ParameterizedSelect[T]): List[Factor[Double]] = {
    val (selectVar, probs) = parameterizedSelectVarAndProbs(select)
    List(makeSimpleDistribution(selectVar, probs))
  }

  private def makeDontCares[U](factor: Factor[Double],
    intermedIndex: Int,
    overallVar: Variable[U],
    outcomeVar: Variable[U]): Unit = {
    // If we don't care, we assign 1.0 to all combinations of the distVar and outcomeVar
    for {
      j <- 0 until overallVar.size
      k <- 0 until outcomeVar.size
    } {
      factor.set(List(intermedIndex, j, k), 1.0)
    }
  }

  private def makeCares[U](factor: Factor[Double], intermedIndex: Int,
    overallVar: Variable[U], outcomeVar: Variable[U], choices: Set[U])(implicit mapper: PointMapper[U]): Unit = {
    // We care to match up distVar with outcomeVar
    for {
      (overallVal, j) <- overallVar.range.zipWithIndex
      (outcomeVal, k) <- outcomeVar.range.zipWithIndex
    } {
      val entry = if (overallVal == mapper.map(outcomeVal, choices)) 1.0; else 0.0
      factor.set(List(intermedIndex, j, k), entry)
    }
  }

    /*
   * The conditional selector creates a factor in which, when the selector's value is such that the result
   * element is relevant to the final result, the result element and overall element must have the same
   * value (handled by makeCares). Otherwise, the result element and overall element can take on any
   * value (handled by makeDontCares)
   */
    /**
   * Make a conditional selector factor used in the decomposition of chain and other elements.
   * A chain defines a factor over the parent element, each of the possible result elements of the chain,
   * and the overall chain element. This can produce a very large factor when there are many result elements.
   * This is solved by decomposing the chain factor into a product of factors, each of which contains the
   * parent element, one of the result elements, and the overall chain element.
   */
  def makeConditionalSelector[T, U](overallElem: Element[U], selector: Variable[T],
    outcomeIndex: Int, outcomeElem: Element[U])(implicit mapper: PointMapper[U]): Factor[Double] = {
    val overallVar = Variable(overallElem)
    val outcomeVar = Variable(outcomeElem)
    val factor = new Factor[Double](List(selector, overallVar, outcomeVar))
    for { i <- 0 until outcomeIndex } { makeDontCares(factor, i, overallVar, outcomeVar) }
    makeCares(factor, outcomeIndex, overallVar, outcomeVar, Values(overallElem.universe)(overallElem))(mapper)
    for { i <- outcomeIndex + 1 until selector.size } { makeDontCares(factor, i, overallVar, outcomeVar) }
    factor
  }

  private def intermedAndClauseFactors[U, T](dist: Dist[U, T]): (Variable[Int], List[Factor[Double]]) = {
    val intermed = new Variable((0 until dist.clauses.size).toList)
    val clauseFactors = dist.outcomes.zipWithIndex map (pair =>
      makeConditionalSelector(dist, intermed, pair._2, pair._1))
    (intermed, clauseFactors)
  }

  private def makeFactors[T](dist: AtomicDist[T]): List[Factor[Double]] = {
    val (intermed, clauseFactors) = intermedAndClauseFactors(dist)
    val intermedFactor = makeSimpleDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  private def makeFactors[T](dist: CompoundDist[T]): List[Factor[Double]] = {
    val (intermed, clauseFactors) = intermedAndClauseFactors(dist)
    val intermedFactor = makeComplexDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  private def makeFactors[T, U](chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val chainMap: Map[T, Element[U]] = Expand(chain.universe).getMap(chain)
    val parentVar = Variable(chain.parent)
    parentVar.range.zipWithIndex map (pair =>
      makeConditionalSelector(chain, parentVar, pair._2, chainMap(pair._1))(mapper))
  }

  private def makeFactors[T, U](apply: Apply1[T, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val arg1Var = Variable(apply.arg1)
    val resultVar = Variable(apply)
    val factor = new Factor[Double](List(arg1Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      result = mapper.map(apply.fn(arg1Val), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry = if (resultVal == result) 1.0; else 0.0
      factor.set(List(arg1Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, U](apply: Apply2[T1, T2, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val resultVar = Variable(apply)
    val factor = new Factor[Double](List(arg1Var, arg2Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      result = mapper.map(apply.fn(arg1Val, arg2Val), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry = if (resultVal == result) 1.0; else 0.0
      factor.set(List(arg1Index, arg2Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, U](apply: Apply3[T1, T2, T3, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val resultVar = Variable(apply)
    val factor = new Factor[Double](List(arg1Var, arg2Var, arg3Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      result = mapper.map(apply.fn(arg1Val, arg2Val, arg3Val), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry = if (resultVal == result) 1.0; else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, T4, U](apply: Apply4[T1, T2, T3, T4, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val resultVar = Variable(apply)
    val factor = new Factor[Double](List(arg1Var, arg2Var, arg3Var, arg4Var, resultVar))
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
      result = mapper.map(apply.fn(arg1Val, arg2Val, arg3Val, arg4Val), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry = if (resultVal == result) 1.0; else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, T4, T5, U](apply: Apply5[T1, T2, T3, T4, T5, U])(implicit mapper: PointMapper[U]): List[Factor[Double]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val arg5Var = Variable(apply.arg5)
    val resultVar = Variable(apply)
    val factor = new Factor[Double](List(arg1Var, arg2Var, arg3Var, arg4Var, arg5Var, resultVar))
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
      result = mapper.map(apply.fn(arg1Val, arg2Val, arg3Val, arg4Val, arg5Val), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val entry = if (resultVal == result) 1.0; else 0.0
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, arg5Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T](inject: Inject[T]): List[Factor[Double]] = {
    def rule(values: List[Any]) = {
      val resultValue :: inputValues = values
      if (resultValue.asInstanceOf[List[T]].toList == inputValues) 1.0
      else 0.0
    }
    val inputVariables = inject.args map (Variable(_))
    val resultVariable = Variable(inject)
    val variables = resultVariable :: inputVariables
    val factor = new Factor[Double](variables)
    factor.fillByRule(rule _)
    List(factor)
  }

  // When we're using a parameter to compute expected sufficient statistics, we just use its expected value
  private def makeFactors(param: Parameter[_]): List[Factor[Double]] = {
    makeFactors(Constant(param.expectedValue))
  }

  private def makeFactors(flip: ParameterizedFlip): List[Factor[Double]] = {
    val flipVar = Variable(flip)
    val factor = new Factor[Double](List(flipVar))
    val prob = flip.parameter.expectedValue
    val i = flipVar.range.indexOf(true)
    factor.set(List(i), prob)
    factor.set(List(1 - i), 1.0 - prob)
    List(factor)
  }

  private def concreteFactors[T](elem: Element[T]): List[Factor[Double]] =
    elem match {
      case f: ParameterizedFlip => makeFactors(f)
      case s: ParameterizedSelect[_] => makeFactors(s)
      case p: Parameter[_] => makeFactors(p)
      case c: Constant[_] => makeFactors(c)
      case f: AtomicFlip => makeFactors(f)
      case f: CompoundFlip => makeFactors(f)
      case s: AtomicSelect[_] => makeFactors(s)
      case s: CompoundSelect[_] => makeFactors(s)
      case d: AtomicDist[_] => makeFactors(d)
      case d: CompoundDist[_] => makeFactors(d)
      case c: Chain[_, _] => makeFactors(c)
      case a: Apply1[_, _] => makeFactors(a)
      case a: Apply2[_, _, _] => makeFactors(a)
      case a: Apply3[_, _, _, _] => makeFactors(a)
      case a: Apply4[_, _, _, _, _] => makeFactors(a)
      case a: Apply5[_, _, _, _, _, _] => makeFactors(a)
      case i: Inject[_] => makeFactors(i)
      case f: ProbFactorMaker => f.makeFactors

      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  private def makeAbstract[T](atomic: Atomic[T], abstraction: Abstraction[T]): List[Factor[Double]] = {
    val variable = Variable(atomic)
    val values = variable.range
    val densityMap = scala.collection.mutable.Map[T, Double]()
    for { v <- values } {
      val currentDensity = densityMap.getOrElse(v, 0.0)
      densityMap.update(v, currentDensity + atomic.density(v))
    }
    val factor = new Factor[Double](List(variable))
    for { (v, i) <- values.zipWithIndex } {
      factor.set(List(i), densityMap(v))
    }
    List(factor)
  }

  private def makeAbstract[T](elem: Element[T], abstraction: Abstraction[T]): List[Factor[Double]] =
    elem match {
      case atomic: Atomic[_] => makeAbstract(atomic, abstraction)
      case apply: Apply1[_, _] => makeFactors(apply)(abstraction.scheme)
      case apply: Apply2[_, _, _] => makeFactors(apply)(abstraction.scheme)
      case apply: Apply3[_, _, _, _] => makeFactors(apply)(abstraction.scheme)
      // In the case of a Chain, its pragmas are inherited by the expanded result elements. The abstraction will be
      // taken into account when we generate factors for the result elements.
      case chain: Chain[_, _] => makeFactors(chain)(abstraction.scheme)
      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  private def makeNonConstraintFactors[T](elem: Element[T]): List[Factor[Double]] =
    Abstraction.fromPragmas(elem.pragmas) match {
      case None => concreteFactors(elem)
      case Some(abstraction) => makeAbstract(elem, abstraction)
    }

  private def makeConditionAndConstraintFactors[T](elem: Element[T]): List[Factor[Double]] =
    elem.allConditions.map(makeConditionFactor(elem, _)) ::: elem.allConstraints.map(makeConstraintFactor(elem, _))

  private def makeConditionFactor[T](elem: Element[T], cc: (T => Boolean, Element.Contingency)): Factor[Double] =
    makeConstraintFactor(elem, ((t: T) => if (cc._1(t)) 1.0; else 0.0, cc._2))

  private def makeConstraintFactor[T](elem: Element[T], cc: (T => Double, Element.Contingency)): Factor[Double] = {
    val (constraint, contingency) = cc
    contingency match {
      case List() => makeUncontingentConstraintFactor(elem, constraint)
      case first :: rest => makeContingentConstraintFactor(elem, constraint, first, rest)
    }
  }

  private def makeUncontingentConstraintFactor[T](elem: Element[T], constraint: T => Double): Factor[Double] = {
    val elemVar = Variable(elem)
    val factor = new Factor[Double](List(elemVar))
    for { (elemVal, index) <- elemVar.range.zipWithIndex } {
      val entry = constraint(elemVal)
      factor.set(List(index), entry)
    }
    factor
  }

  private def makeContingentConstraintFactor[T](elem: Element[T], constraint: T => Double, firstConting: Element.ElemVal[_], restContinges: Element.Contingency): Factor[Double] = {
    val restFactor = makeConstraintFactor(elem, (constraint, restContinges))
    extendConstraintFactor(restFactor, firstConting)
  }

  private def extendConstraintFactor(restFactor: Factor[Double], firstConting: Element.ElemVal[_]): Factor[Double] = {
    // The extended factor is obtained by getting the underlying factor and expanding each row so that the row only provides its entry if the contingent variable takes
    // on the appropriate value, otherwise the entry is 1
    val Element.ElemVal(firstElem, firstValue) = firstConting
    val firstVar = Variable(firstElem)
    val firstValues = firstVar.range
    val numFirstValues = firstValues.size
    val matchingIndex: Int = firstValues.indexOf(firstValue)
    val resultFactor = new Factor[Double](firstVar :: restFactor.variables)
    for { restIndices <- restFactor.allIndices } {
      val restEntry = restFactor.get(restIndices)
      for { firstIndex <- 0 until numFirstValues } {
        val resultEntry = if (firstIndex == matchingIndex) restEntry; else 1.0
        resultFactor.set(firstIndex :: restIndices, resultEntry)
      }
    }
    resultFactor
  }

  private val factorCache = scala.collection.mutable.Map[Element[_], List[Factor[Double]]]()

  /**
   * Create the probabilistic factors associated with an element. This method is memoized.
   */
  def make(elem: Element[_]): List[Factor[Double]] = {
    val nonConstraintFactors =
      factorCache.get(elem) match {
        case Some(l) => l
        case None =>
          val result = makeNonConstraintFactors(elem)
          factorCache += elem -> result
          elem.universe.register(factorCache)
          result
      }
    makeConditionAndConstraintFactors(elem) ::: nonConstraintFactors
  }

  /**
   * Remove an element from the factor cache, ensuring that factors for the element
   * are regenerated. This is important, for example,  if evidence on the variable has changed. 
   * 
   */
  def removeFactors(elem: Element[_]) { factorCache -= elem }
  
  /**
   * Clear the factor cache.
   */
  def removeFactors() { factorCache.clear }

  /**
   * Create the probabilistic factor encoding the probability of evidence in the dependent universe as a function of the
   * values of variables in the parent universe. The third argument is the the function to use for computing
   * probability of evidence in the dependent universe. It is assumed that the definition of this function will already contain the
   * right evidence.
   */
  def makeDependentFactor(parentUniverse: Universe,
    dependentUniverse: Universe,
    probEvidenceComputer: () => Double): Factor[Double] = {
    val uses = dependentUniverse.parentElements filter (_.universe == parentUniverse)
    def rule(values: List[Any]) = {
      for { (elem, value) <- uses zip values } { elem.value = value.asInstanceOf[elem.Value] }
      val result = probEvidenceComputer()
      result
    }
    val variables = uses map (Variable(_))
    val factor = new Factor[Double](variables)
    factor.fillByRule(rule _)
    factor
  }
}
