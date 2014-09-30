/*
 * SufficientStatisticsVariableElimination.scala
 * Variable elimination algorithm for sufficient statistics factors
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.language._
import scala.collection._
import scala.collection.mutable.{ Map, Set }

/**
 * Variable elimination for sufficient statistics factors. 
 * The final factor resulting from variable elimination contains a mapping of parameters to sufficient statistics vectors
 * which can be used to maximize parameter values.
 * 
 * @param parameterMap A map of parameters to their sufficient statistics.
 */
class SufficientStatisticsVariableElimination(
  parameterMap: immutable.Map[Parameter[_], Seq[Double]],
  val universe: Universe)
  extends VariableElimination[(Double, Map[Parameter[_], Seq[Double]])] {

  /**
   * No timing information enabled for this algorithm.
   */
  val showTiming = false

  protected val statFactor = new SufficientStatisticsFactor(parameterMap)

  /**
   * Clear the sufficient statistics factors used by this algorithm.
   */
  private def removeFactors() {
    statFactor.removeFactors
  }

  /**
   *  Particular implementations of probability of evidence algorithms must define the following method. 
   */
  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[(Double, mutable.Map[Parameter[_], Seq[Double]])]] = {
    val allElements = neededElements.filter(p => p.isInstanceOf[Parameter[_]] == false)
    if (debug) {
      println("Elements appearing in factors and their ranges:")
      for { element <- allElements } { 
        println(Variable(element).id + "(" + element.name.string + "@" + element.hashCode + ")" + ": " + element + ": " + Variable(element).range.mkString(",")) 
      }
    }

    Factory.removeFactors()
    val thisUniverseFactors = allElements flatMap (statFactor.make(_))
    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield statFactor.makeDependentFactor(universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))

    dependentUniverseFactors ::: thisUniverseFactors
  }

  /**
   * Empty for this algorithm.
   */
  val targetElements = List[Element[_]]()
  
  override def starterElements = universe.conditionedElements ++ universe.constrainedElements

  private var result: (Double, Map[Parameter[_], Seq[Double]]) = _

  def finish(factorsAfterElimination: Set[Factor[(Double, Map[Parameter[_], Seq[Double]])]], eliminationOrder: List[Variable[_]]): Unit = {
    // It is possible that there are no factors (this will happen if there is no evidence).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    val finalFactor = factorsAfterElimination.foldLeft(Factor.unit(semiring))(_.product(_, semiring))
    finalFactor.variables.size match {
      case 0 => result = finalFactor.get(List())
      case _ => throw new RuntimeException("Final factor has variables")
    }
  }

  /**
   * Returns a mapping of parameters to sufficient statistics resulting from 
   * elimination of the factors.
   */
  def getSufficientStatisticsForAllParameters = { result._2.toMap }

  val semiring = SufficientStatisticsSemiring(parameterMap)

  override def cleanUp() = { 
    statFactor.removeFactors
    super.cleanUp()
  }

  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])] = List()
  val dependentAlgorithm = (u: Universe, e: List[NamedEvidence[_]]) => () => 1.0

}

object SufficientStatisticsVariableElimination {
  def apply(parameterMap : immutable.Map[Parameter[_], Seq[Double]])(implicit universe: Universe) = new SufficientStatisticsVariableElimination(parameterMap,universe)
}
