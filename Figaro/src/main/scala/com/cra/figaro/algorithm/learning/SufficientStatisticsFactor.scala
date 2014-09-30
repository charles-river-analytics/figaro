/*
 * SufficientStatisticsFactor.scala
 * Factors over variables and their sufficient statistics
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.learning

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.library.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.ParameterizedBinomialFixedNumTrials
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection._
import scala.collection.mutable.{ Set, Map }
import scala.math.{ floor, pow }
import JSci.maths.ExtraMath.binomial

/**
 * Methods for creating probabilistic factors associated with elements and their sufficient statistics.
 * 
 * @param parameterMap Map of parameters to their sufficient statistics. Expectation
 */

class SufficientStatisticsFactor(parameterMap: immutable.Map[Parameter[_], Seq[Double]]) 
{

  private def makeFactors[T](const: Constant[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(Variable(const)))
    val mapping = mutable.Map(parameterMap.toSeq: _*)
    factor.set(List(0), (1.0, mapping))
    List(factor)
  }

  private def makeFactors(flip: AtomicFlip): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val flipVar = Variable(flip)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(flipVar))
    val i = flipVar.range.indexOf(Regular(true))
    val trueMapping = mutable.Map(parameterMap.toSeq: _*)
    val falseMapping = mutable.Map(parameterMap.toSeq: _*)
    factor.set(List(i), (flip.prob, trueMapping))
    factor.set(List(1 - i), (1 - flip.prob, falseMapping))
    List(factor)
  }

  private def makeFactors(flip: CompoundFlip): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val flipVar = Variable(flip)
    val probVar = Variable(flip.prob)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(probVar, flipVar))
    val parentVals = probVar.range
    val i = flipVar.range.indexOf(Regular(true))

    for { j <- 0 until parentVals.size } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      factor.set(List(j, i), (parentVals(j).value, rowMapping))
      factor.set(List(j, 1 - i), (1.0 - parentVals(j).value, rowMapping))
    }
    List(factor)
  }

  private def makeFactors(flip: ParameterizedFlip): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val flipVar = Variable(flip)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(flipVar))
    val prob = flip.parameter.MAPValue
    val i = flipVar.range.indexOf(Regular(true))

    val falseMapping = mutable.Map(parameterMap.toSeq: _*)
    val trueMapping = mutable.Map(parameterMap.toSeq: _*)

    trueMapping.remove(flip.parameter)
    falseMapping.remove(flip.parameter)
    trueMapping.put(flip.parameter, Seq(1.0, 0.0))
    falseMapping.put(flip.parameter, Seq(0.0, 1.0))
    factor.set(List(i), (prob, trueMapping))
    factor.set(List(1 - i), (1.0 - prob, falseMapping))

    List(factor)
  }

  private def makeFactors(bin: ParameterizedBinomialFixedNumTrials): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val binVar = Variable(bin)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(binVar))
    val prob = bin.parameter.MAPValue.asInstanceOf[Double]
    val mappings = binVar.range.map(i => (i, mutable.Map(parameterMap.toSeq: _*)))
    for { 
      (ext, map) <- mappings
      if (ext.isRegular)
    } { 
      val i = ext.value
      map.remove(bin.parameter)
      map.put(bin.parameter, Seq(i, bin.numTrials - i))
      val density = binomial(bin.numTrials, i) * pow(prob, i) * pow(1 - prob, bin.numTrials - i)
      val index = binVar.range.indexOf(ext)
      factor.set(List(index), (density, map))
    }
    List(factor)
  }

  private def makeSimpleDistribution[T](target: Variable[T], probs: List[Double]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {

    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(target))
    for { (prob, index) <- probs.zipWithIndex } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      factor.set(List(index), (prob, rowMapping))
    }
    factor
  }

  private def makeSimpleDistributionForParameterized[T](target: Variable[T], probs: List[Double], select: ParameterizedSelect[T]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(target))
    //For each outcome
    val unzippedClauses = select.clauses.unzip

    for { (prob, probindex) <- probs.zipWithIndex } {
      //Row is a vector of zeros for all parameters
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      //Remove the default entry for this parameter
      rowMapping.remove(select.parameter)
      //The index in the parameter vector doesn't necessarily match the range index.
      //It must be retrieved from the target.
      val varIndex = unzippedClauses._2.indexOf(target.range(probindex).value)

      val entry = select.parameter.zeroSufficientStatistics.updated(varIndex, 1.0)

      rowMapping.put(select.parameter, entry)
      factor.set(List(probindex), (prob, rowMapping))
    }
    factor
  }

  private def makeComplexDistribution[T](target: Variable[T], probElems: List[Element[Double]]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val probVars = probElems map (Variable(_))
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List((target :: probVars): _*))
    val probVals = probVars map (_.range)
    for { indices <- factor.allIndices } {
      val probIndices = indices.toList.tail.zipWithIndex
      val unnormalized = probIndices map (pair => probVals(pair._2)(pair._1).value)
      val normalized = normalize(unnormalized).toArray
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)

      factor.set(indices, (normalized(indices(0)), rowMapping))
    }
    factor
  }

  private def selectVarAndProbs[U, T](select: Select[U, T]): (Variable[T], List[U]) = {
    val selectVar = Variable(select)

    val probs = for { xvalue <- selectVar.range } yield select.clauses.find(_._2 == xvalue.value).get._1
    (selectVar, probs)
  }

  private def selectVarAndProbs[U, T](select: ParameterizedSelect[T]): (Variable[T], List[Double]) = {
    val selectVar = Variable(select)
    val unzippedClauses = select.clauses.unzip
    val MAPValue = select.parameter.MAPValue
    val probs = for { xvalue <- selectVar.range } yield MAPValue(unzippedClauses._2.indexOf(xvalue.value))
    val result = (selectVar, probs)
    result
  }

  private def makeFactors[T](select: AtomicSelect[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {

    val (selectVar, probs) = selectVarAndProbs(select)
    List(makeSimpleDistribution(selectVar, probs))
  }

  private def makeFactors[T](select: CompoundSelect[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val (selectVar, probs) = selectVarAndProbs(select)
    List(makeComplexDistribution(selectVar, probs))
  }

  private def makeFactors[T](select: ParameterizedSelect[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val (selectVar, probs) = selectVarAndProbs(select)
    List(makeSimpleDistributionForParameterized(selectVar, probs, select))
  }

  private def makeDontCares[U](factor: Factor[(Double, Map[Parameter[_], Seq[Double]])],
    intermedIndex: Int,
    overallVar: Variable[U],
    outcomeVar: Variable[U]): Unit = {
    // If we don't care, we assign 1.0 to all combinations of the distVar and outcomeVar
    for {
      j <- 0 until overallVar.size
      k <- 0 until outcomeVar.size
    } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      factor.set(List(intermedIndex, j, k), (1.0, rowMapping))
    }
  }

  private def makeCares[U](factor: Factor[(Double, Map[Parameter[_], Seq[Double]])], intermedIndex: Int,
    overallVar: Variable[U], outcomeVar: Variable[U], choices: scala.collection.immutable.Set[U])(implicit mapper: PointMapper[U]): Unit = {
    // We care to match up distVar with outcomeVar
    for {
      (overallVal, j) <- overallVar.range.zipWithIndex
      (outcomeVal, k) <- outcomeVar.range.zipWithIndex
    } {

      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      val entry = if (overallVal.value == mapper.map(outcomeVal.value, choices)) (1.0, rowMapping); else (0.0, rowMapping)
      factor.set(List(intermedIndex, j, k), entry)
    }
  }


  /**
   * Make a conditional selector factor used in the decomposition of chain and other elements.
   * A chain defines a factor over the parent element, each of the possible result elements of the chain,
   * and the overall chain element. This can produce a very large factor when there are many result elements.
   * This is solved by decomposing the chain factor into a product of factors, each of which contains the
   * parent element, one of the result elements, and the overall chain element.
   */
  def makeConditionalSelector[T, U](overallElem: Element[U], selector: Variable[T],
    outcomeIndex: Int, resultElem: Element[U])(implicit mapper: PointMapper[U]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val overallVar = Variable(overallElem)

    val outcomeVar = Variable(resultElem)

    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(selector, overallVar, outcomeVar))
    for { i <- 0 until outcomeIndex } { makeDontCares(factor, i, overallVar, outcomeVar) }
    makeCares(factor, outcomeIndex, overallVar, outcomeVar, Values(overallElem.universe)(overallElem))(mapper)
    for { i <- outcomeIndex + 1 until selector.size } { makeDontCares(factor, i, overallVar, outcomeVar) }
    factor
  }

  private def intermedAndClauseFactors[U, T](dist: Dist[U, T]): (Variable[Int], List[Factor[(Double, Map[Parameter[_], Seq[Double]])]]) = {
    val intermed = new Variable(ValueSet.withoutStar((0 until dist.clauses.size).toSet))
    val clauseFactors = dist.outcomes.zipWithIndex map (pair =>
      makeConditionalSelector(dist, intermed, pair._2, pair._1))
    (intermed, clauseFactors)
  }

  private def makeFactors[T](dist: AtomicDist[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {

    val (intermed, clauseFactors) = intermedAndClauseFactors(dist)
    val intermedFactor = makeSimpleDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  private def makeFactors[T](dist: CompoundDist[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {

    val (intermed, clauseFactors) = intermedAndClauseFactors(dist)
    val intermedFactor = makeComplexDistribution(intermed, dist.probs)
    intermedFactor :: clauseFactors
  }

  private def makeFactors[T, U](chain: Chain[T, U])(implicit mapper: PointMapper[U]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {

    val chainMap: scala.collection.mutable.Map[T, Element[U]] = LazyValues(chain.universe).getMap(chain)
    val parentVar = Variable(chain.parent)
    parentVar.range.zipWithIndex map (pair =>
      makeConditionalSelector(chain, parentVar, pair._2, chainMap(pair._1.value))(mapper))
  }

  private def makeFactors[T, U](apply: Apply1[T, U])(implicit mapper: PointMapper[U]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {

    val arg1Var = Variable(apply.arg1)
    val resultVar = Variable(apply)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(arg1Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      result = mapper.map(apply.fn(arg1Val.value), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      val entry = if (resultVal.value == result) (1.0, rowMapping); else (0.0, rowMapping)
      factor.set(List(arg1Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, U](apply: Apply2[T1, T2, U])(implicit mapper: PointMapper[U]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {

    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val resultVar = Variable(apply)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(arg1Var, arg2Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      result = mapper.map(apply.fn(arg1Val.value, arg2Val.value), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)

      val entry = if (resultVal.value == result) (1.0, rowMapping); else (0.0, rowMapping)
      factor.set(List(arg1Index, arg2Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, U](apply: Apply3[T1, T2, T3, U])(implicit mapper: PointMapper[U]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val resultVar = Variable(apply)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(arg1Var, arg2Var, arg3Var, resultVar))
    val arg1Indices = arg1Var.range.zipWithIndex
    val arg2Indices = arg2Var.range.zipWithIndex
    val arg3Indices = arg3Var.range.zipWithIndex
    val resultIndices = resultVar.range.zipWithIndex
    for {
      (arg1Val, arg1Index) <- arg1Indices
      (arg2Val, arg2Index) <- arg2Indices
      (arg3Val, arg3Index) <- arg3Indices
      result = mapper.map(apply.fn(arg1Val.value, arg2Val.value, arg3Val.value), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      val entry = if (resultVal.value == result) (1.0, rowMapping); else (0.0, rowMapping)
      factor.set(List(arg1Index, arg2Index, arg3Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, T4, U](apply: Apply4[T1, T2, T3, T4, U])(implicit mapper: PointMapper[U]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val resultVar = Variable(apply)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(arg1Var, arg2Var, arg3Var, arg4Var, resultVar))
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
      result = mapper.map(apply.fn(arg1Val.value, arg2Val.value, arg3Val.value, arg4Val.value), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      val entry = if (resultVal.value == result) (1.0, rowMapping); else (0.0, rowMapping)
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T1, T2, T3, T4, T5, U](apply: Apply5[T1, T2, T3, T4, T5, U])(implicit mapper: PointMapper[U]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val arg1Var = Variable(apply.arg1)
    val arg2Var = Variable(apply.arg2)
    val arg3Var = Variable(apply.arg3)
    val arg4Var = Variable(apply.arg4)
    val arg5Var = Variable(apply.arg5)
    val resultVar = Variable(apply)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(arg1Var, arg2Var, arg3Var, arg4Var, arg5Var, resultVar))
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
      result = mapper.map(apply.fn(arg1Val.value, arg2Val.value, arg3Val.value, arg4Val.value, arg5Val.value), Values(apply.universe)(apply))
      (resultVal, resultIndex) <- resultIndices
    } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)

      val entry = if (resultVal.value == result) (1.0, rowMapping); else (0.0, rowMapping)
      factor.set(List(arg1Index, arg2Index, arg3Index, arg4Index, arg5Index, resultIndex), entry)
    }
    List(factor)
  }

  private def makeFactors[T](inject: Inject[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    def blankRule(values: List[Any]) = {
      val resultValue :: inputValues = values

      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      if (resultValue.asInstanceOf[List[T]].toList == inputValues) (1.0, rowMapping)
      else (0.0, rowMapping)
    }

    def parameterRule(values: List[T], p: ParameterizedVariable[T]) = {
      val resultValue :: inputValues = values

      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      rowMapping.remove(p.element.parameter)
      if (resultValue.asInstanceOf[List[T]].toList == inputValues) {
        rowMapping.put(p.element.parameter, p.sufficientStatistics(resultValue))
      } else {
        rowMapping.put(p.element.parameter, p.sufficientStatistics(resultValue))
      }
      if (resultValue.asInstanceOf[List[T]].toList == inputValues) (1.0, rowMapping)
      else (0.0, rowMapping)
    }

    val inputVariables = inject.args map (Variable(_))
    val resultVariable = Variable(inject)
    val variables = resultVariable :: inputVariables
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](variables)

    val ranges: List[(List[(Any, Int)], Int)] = List()

    val mapping = Map.empty[Int, Variable[_]]
    for (v <- variables) {
      mapping.put(v.id, v)
      ranges :: List((v.range.zipWithIndex, v.id))
    }

    val newRanges: List[List[(Any, Int, Int)]] = List()
    for (l <- ranges) {
      newRanges :: l._1.map(a => (a._1, a._2, l._2))
    }


    val cases: List[List[Any]] = cartesianProduct(newRanges: _*)
    for { cas <- cases } {
      //Unzip splits into a list of values and a list of indices
      val values: List[Any] = List()
      val indices: List[Int] = List()
      val ids: List[Int] = List()
      for (a: (Any, Int, Int) <- cas.asInstanceOf[List[(Any, Int, Int)]]) {
        indices :+ a._1 //Very slow...
        values :+ a._2
        ids :+ a._3
      }
      mapping(ids(0)) match {
        case p: ParameterizedVariable[T] => {
          factor.set(indices, parameterRule(values.asInstanceOf[List[Extended[T]]].map(_.value), p))
        }
        case v: Variable[_] => factor.set(indices, blankRule(values))
      }

    }

    List(factor)
  }

  private def convertProbFactor(probFactor: Factor[Double]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val result = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](probFactor.variables)
    for { indices <- result.allIndices } {
      result.set(indices, (probFactor.get(indices), mutable.Map(parameterMap.toSeq: _*)))
    }
    result
  }

  private def concreteFactors[T](elem: Element[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] =
    elem match {
      case c: Constant[_] => makeFactors(c)
      case f: AtomicFlip => makeFactors(f)
      case f: CompoundFlip => makeFactors(f)
      case f: ParameterizedFlip => makeFactors(f)
      case s: ParameterizedSelect[_] => makeFactors(s)
      case b: ParameterizedBinomialFixedNumTrials => makeFactors(b)
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
      case f: ProbFactorMaker => 
        Factory.concreteFactors(f).map(convertProbFactor(_))
      /*case p: Parameter[_] => makeFactors(p)*/
      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  private def makeAbstract[T](atomic: Atomic[T], abstraction: Abstraction[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val variable = Variable(atomic)
    val values = variable.range
    val densityMap = scala.collection.mutable.Map[T, Double]()
    for { v <- values } {
      val currentDensity = densityMap.getOrElse(v.value, 0.0)
      densityMap.update(v.value, currentDensity + atomic.density(v.value))
    }
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(variable))
    for { (v, i) <- values.zipWithIndex } {
      val rowMapping = Map.empty[Parameter[_], Seq[Double]] ++ parameterMap
      factor.set(List(i), (densityMap(v.value), rowMapping))
    }
    List(factor)
  }

  private def makeAbstract[T](elem: Element[T], abstraction: Abstraction[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] =
    elem match {
      case atomic: Atomic[_] => makeAbstract(atomic, abstraction)
      case apply: Apply1[_, _] => makeFactors(apply)(abstraction.scheme)
      case apply: Apply2[_, _, _] => makeFactors(apply)(abstraction.scheme)
      case apply: Apply3[_, _, _, _] => makeFactors(apply)(abstraction.scheme) //Applies are ok.
      // In the case of a Chain, its pragmas are inherited by the expanded result elements. The abstraction will be
      // taken into account when we generate factors for the result elements.
      case chain: Chain[_, _] => makeFactors(chain)(abstraction.scheme)
      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  private def makeNonConstraintFactors[T](elem: Element[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] =
    Abstraction.fromPragmas(elem.pragmas) match {
      case None => concreteFactors(elem)
      case Some(abstraction) => makeAbstract(elem, abstraction)
    }

  private def makeConditionAndConstraintFactors[T](elem: Element[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] =
    elem.allConditions.map(makeConditionFactor(elem, _)) ::: elem.allConstraints.map(makeConstraintFactor(elem, _))

  private def makeConditionFactor[T](elem: Element[T], cc: (T => Boolean, Element.Contingency)): Factor[(Double, Map[Parameter[_], Seq[Double]])] =
    makeConstraintFactor(elem, (ProbConstraintType((t: T) => if (cc._1(t)) 1.0; else 0.0), cc._2))

  private def makeConstraintFactor[T](elem: Element[T], cc: (T => Double, Element.Contingency)): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val (constraint, contingency) = cc
    contingency match {
      case List() => makeUncontingentConstraintFactor(elem, constraint)
      case first :: rest => makeContingentConstraintFactor(elem, constraint, first, rest)
    }
  }

  private def makeUncontingentConstraintFactor[T](elem: Element[T], constraint: T => Double): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val elemVar = Variable(elem)
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](List(elemVar))
    for { (elemVal, index) <- elemVar.range.zipWithIndex } {
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      val entry = (math.exp(constraint(elemVal.value)), rowMapping)
      factor.set(List(index), entry)
    }
    factor
  }

  private def makeContingentConstraintFactor[T](elem: Element[T], constraint: T => Double, firstConting: Element.ElemVal[_], restContinges: Element.Contingency): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val restFactor = makeConstraintFactor(elem, (constraint, restContinges))
    extendConstraintFactor(restFactor, firstConting)
  }

  private def extendConstraintFactor(restFactor: Factor[(Double, Map[Parameter[_], Seq[Double]])], firstConting: Element.ElemVal[_]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    // The extended factor is obtained by getting the underlying factor and expanding each row so that the row only provides its entry if the contingent variable takes
    // on the appropriate value, otherwise the entry is 1
    val Element.ElemVal(firstElem, firstValue) = firstConting
    val firstVar = Variable(firstElem)
    val firstValues = firstVar.range
    val numFirstValues = firstValues.size
    val matchingIndex: Int = firstValues.indexOf(Regular(firstValue))
    val resultFactor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](firstVar :: restFactor.variables)
    for { restIndices <- restFactor.allIndices } {
      val restEntry = restFactor.get(restIndices)._1
      for { firstIndex <- 0 until numFirstValues } {
        val rowMapping = mutable.Map(parameterMap.toSeq: _*)
        val resultEntry = if (firstIndex == matchingIndex) restEntry; else 1.0
        resultFactor.set(firstIndex :: restIndices, (resultEntry, rowMapping))
      }
    }
    resultFactor
  }

  private val factorCache = scala.collection.mutable.Map[Element[_], List[Factor[(Double, Map[Parameter[_], Seq[Double]])]]]()

  /**
   * Create the probabilistic factors associated with an element. This method is memoized.
   */
  def make(elem: Element[_]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
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
   * Removes the factors for the specified element from the cache.
   */
  def removeFactors(elem: Element[_]) { factorCache -= elem }
  /**
   * Removes the factors for all elements from the cache. 
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
    probEvidenceComputer: () => Double): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val uses = dependentUniverse.parentElements filter (_.universe == parentUniverse)
    def rule(values: List[Any]) = {
      for { (elem, value) <- uses zip values } { elem.value = value.asInstanceOf[elem.Value] }
      val rowMapping = mutable.Map(parameterMap.toSeq: _*)
      val result = probEvidenceComputer()
      (result, rowMapping)
    }
    val variables = uses map (Variable(_))
    val factor = Factory.make[(Double, Map[Parameter[_], Seq[Double]])](variables)
    factor.fillByRule(rule _)
    factor
  }

}
