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
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.library.decision._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.ParameterizedBinomialFixedNumTrials
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection._
import scala.collection.mutable.{ Set }
import scala.collection.immutable.Map
import scala.math.{ floor, pow }
import JSci.maths.ExtraMath.binomial
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.algorithm.factored.factors.factory.Factory

/**
 * Methods for creating probabilistic factors associated with elements and their sufficient statistics.
 *
 * @param parameterMap Map of parameters to their sufficient statistics. Expectation
 */

class SufficientStatisticsFactor(parameterMap: Map[Parameter[_], Seq[Double]]) {
  val semiring = new SufficientStatisticsSemiring(parameterMap)

  def convertFactor[T](factor: Factor[Double]): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    def doubleToParam(d: Double) = (d, Map(parameterMap.toSeq: _*))
    factor.mapTo(doubleToParam, semiring)
  }

  def partitionConstraintFactors(factors: List[Factor[Double]]) = factors.partition(_.isConstraint)

  private def makeFactors(flip: ParameterizedFlip): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val origFactor = Factory.makeFactorsForElement(flip, false, true)
    val (constraints, nonconstraints) = partitionConstraintFactors(origFactor)

    val flipVar = Variable(flip)
    val factor = Factory.defaultFactor[(Double, Map[Parameter[_], Seq[Double]])](List(), List(flipVar), semiring)
    val prob = flip.parameter.MAPValue
    val i = flipVar.range.indexOf(Regular(true))

    val falseMapping = Map(parameterMap.toSeq: _*) + ((flip.parameter: Parameter[_]) -> Seq(0.0, 1.0))
    val trueMapping = Map(parameterMap.toSeq: _*) + ((flip.parameter: Parameter[_]) -> Seq(1.0, 0.0))

    factor.set(List(i), (prob, trueMapping))
    factor.set(List(1 - i), (1.0 - prob, falseMapping))

    constraints.map(convertFactor(_)) ++ List(factor)
  }

  private def makeFactors(bin: ParameterizedBinomialFixedNumTrials): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val origFactor = Factory.makeFactorsForElement(bin, false, true)
    val (constraints, nonconstraints) = partitionConstraintFactors(origFactor)

    val binVar = Variable(bin)
    val factor = Factory.defaultFactor[(Double, Map[Parameter[_], Seq[Double]])](List(), List(binVar), semiring)
    val prob = bin.parameter.MAPValue.asInstanceOf[Double]
    val mappings = binVar.range.map(i => (i, Map(parameterMap.toSeq: _*)))
    for {
      ext <- binVar.range
      if (ext.isRegular)
    } {
      val i = ext.value
      val kv: (Parameter[_], Seq[Double]) = (bin.parameter, Seq[Double](i, bin.numTrials - i))
      val map = Map(parameterMap.toSeq: _*) + kv
      val density = binomial(bin.numTrials, i) * pow(prob, i) * pow(1 - prob, bin.numTrials - i)
      val index = binVar.range.indexOf(ext)
      factor.set(List(index), (density, map))
    }
    constraints.map(convertFactor(_)) ++ List(factor)
  }

  private def makeSimpleDistributionForParameterized[T](target: Variable[T], probs: List[Double], select: ParameterizedSelect[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val origFactor = Factory.makeFactorsForElement(select, false, true)
    val (constraints, nonconstraints) = partitionConstraintFactors(origFactor)

    val factor = Factory.defaultFactor[(Double, Map[Parameter[_], Seq[Double]])](List(), List(target), semiring)
    //For each outcome
    val unzippedClauses = select.clauses.unzip

    for { (prob, probindex) <- probs.zipWithIndex } {
      //The index in the parameter vector doesn't necessarily match the range index.
      //It must be retrieved from the target.
      val varIndex = unzippedClauses._2.indexOf(target.range(probindex).value)
      val entry = select.parameter.zeroSufficientStatistics.updated(varIndex, 1.0)
      //Row is a vector of zeros for all parameters
      val rowMapping = Map(parameterMap.toSeq: _*) + ((select.parameter: Parameter[_]) -> entry)
      factor.set(List(probindex), (prob, rowMapping))
    }
    factor :: constraints.map(convertFactor(_))
  }

  private def selectVarAndProbs[U, T](select: ParameterizedSelect[T]): (Variable[T], List[Double]) = {
    val selectVar = Variable(select)
    val unzippedClauses = select.clauses.unzip
    val MAPValue = select.parameter.MAPValue
    val probs = for { xvalue <- selectVar.range } yield MAPValue(unzippedClauses._2.indexOf(xvalue.value))
    val result = (selectVar, probs)
    result
  }

  private def makeFactors[T](select: ParameterizedSelect[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    val (selectVar, probs) = selectVarAndProbs(select)
    makeSimpleDistributionForParameterized(selectVar, probs, select)
  }

  private def makeFactors[T](inject: Inject[T]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    def blankRule(values: List[Any]) = {
      val resultValue :: inputValues = values
      val rowMapping = Map(parameterMap.toSeq: _*)
      if (resultValue.asInstanceOf[List[T]].toList == inputValues) (1.0, rowMapping)
      else (0.0, rowMapping)
    }

    def parameterRule(values: List[T], p: ParameterizedVariable[T]) = {
      val resultValue :: inputValues = values

      val rowMapping = Map(parameterMap.toSeq: _*) ++ {
        if (resultValue.asInstanceOf[List[T]].toList == inputValues) {
          for (pr <- p.element.parameters) yield {
            (pr, pr.sufficientStatistics(resultValue))
          }
        } else {
          for (pr <- p.element.parameters) yield {
            (pr, pr.sufficientStatistics(resultValue))
          }
        }
      }
      if (resultValue.asInstanceOf[List[T]].toList == inputValues) (1.0, rowMapping)
      else (0.0, rowMapping)
    }

    val origFactor = Factory.makeFactorsForElement(inject, false, true)
    val (constraints, nonconstraints) = partitionConstraintFactors(origFactor)

    val inputVariables = inject.args map (Variable(_))
    val resultVariable = Variable(inject)
    val factor = Factory.defaultFactor[(Double, Map[Parameter[_], Seq[Double]])](inputVariables, List(resultVariable), semiring)
    val variables = factor.variables

    val ranges: List[(List[(Any, Int)], Int)] = List()

    val mapping = Map.empty[Int, Variable[_]] ++ {
      for (v <- variables) yield {
        ranges :: List((v.range.zipWithIndex, v.id))
        (v.id, v)
      }
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

    factor :: constraints.map(convertFactor(_))
  }

  /**
   * Create the probabilistic factors associated with an element. This method is memoized.
   */
  def make(elem: Element[_]): List[Factor[(Double, Map[Parameter[_], Seq[Double]])]] = {
    elem match {
      case f: ParameterizedFlip => makeFactors(f)
      case s: ParameterizedSelect[_] => makeFactors(s)
      case b: ParameterizedBinomialFixedNumTrials => makeFactors(b)
      case i: Inject[_] => makeFactors(i)
      case _ => {
        val origFactor = Factory.makeFactorsForElement(elem)
        origFactor.map(convertFactor(_))
      }
    }
  }

  /**
   * Create the probabilistic factor encoding the probability of evidence in the dependent universe as a function of the
   * values of variables in the parent universe. The third argument is the the function to use for computing
   * probability of evidence in the dependent universe. It is assumed that the definition of this function will already contain the
   * right evidence.
   */
  def makeDependentFactor(cc: ComponentCollection, parentUniverse: Universe,
    dependentUniverse: Universe,
    probEvidenceComputer: () => Double): Factor[(Double, Map[Parameter[_], Seq[Double]])] = {
    val factor = Factory.makeDependentFactor(cc, parentUniverse, dependentUniverse, probEvidenceComputer)
    convertFactor(factor)
    
  }

}
