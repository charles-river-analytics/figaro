/*
 * LazyVE.scala
 * Lazy variable elimination algorithm.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Dec 28, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.lazyfactored

import com.cra.figaro.algorithm.{LazyAlgorithm, ProbQueryAlgorithm}
import com.cra.figaro.language.{Element, Universe}
import com.cra.figaro.algorithm.factored._
import scala.annotation.tailrec
import com.cra.figaro.util._
import scala.collection.mutable.{Map, Set}

class LazyVariableElimination(targetElements: Element[_]*)(implicit val universe: Universe) extends FactoredAlgorithm[Double] 
with LazyAlgorithm {
  var debug = false
  var showTiming = false
  
  val dependentAlgorithm = null
  
  val dependentUniverses = List()
  
  val semiring = SumProductSemiring
  
  var currentResult: Factor[(Double, Double)] = _

  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upperBounds: Boolean = false): List[Factor[Double]] = {
    for {
      elem <- neededElements
      factor <- BoundedProbFactor.make(elem, upperBounds)
      if !factor.isEmpty
    } yield factor
  }

  private def optionallyShowTiming[T](op: => T, name: String) =
    if (showTiming) timed(op, name); else op

  def run(depth: Int) {
    Variable.clearCache()
    val (includedElements, needsBounds) = getNeededElements(targetElements.toList, depth)
    val targetVariables = targetElements.map(Variable(_))
    if (needsBounds) {
      Factory.removeFactors()
      val lowerFactors = getFactors(includedElements, targetElements.toList, false)
      Factory.removeFactors()
      val upperFactors = getFactors(includedElements, targetElements.toList, true)
      val lowerFactorsAfterElimination = doElimination(lowerFactors, targetVariables)
      val upperFactorsAfterElimination = doElimination(upperFactors, targetVariables)
      currentResult = finishWithBounds(lowerFactorsAfterElimination, upperFactorsAfterElimination)
    } else {
      Factory.removeFactors()
      val allFactors = getFactors(includedElements, targetElements.toList)
      val factorsAfterElimination = doElimination(allFactors, targetVariables)
      currentResult = finishNoBounds(factorsAfterElimination)
    }
    marginalize(currentResult)
  }
  
  protected def doElimination(allFactors: List[Factor[Double]], targetVariables: Seq[Variable[_]]): Set[Factor[Double]] = {
    val allVars = (Set[/*Extended*/Variable[_]]() /: allFactors)(_ ++ _./*extended*/variables)
    if (debug) {
      println("*****************\nElement ids:")
      for { variable <- allVars } {
        variable match {
          case elemVar: /*Extended*/ElementVariable[_] =>
            println(variable.id + "(" + elemVar.element.name.string + ")" + "@" + elemVar.element.hashCode + ": " + elemVar.element) 
          case _ =>
            println(variable.id + ": not an element variable")
        }
      }
    }
    recordingFactors = List()
    if (debug) {
      println("*****************\nStarting factors\n")
      allFactors.foreach((f: /*Extended*/Factor[_]) => println(f.toReadableString))
    }
    val order = optionallyShowTiming(eliminationOrder(allVars, allFactors, targetVariables), "Computing elimination order")
    val factorsAfterElimination =
      optionallyShowTiming(eliminateInOrder(order, Set(allFactors: _*), initialFactorMap(allFactors)), "Elimination")
    if (debug) println("*****************")
    if (debug) factorsAfterElimination foreach (f => println(f.toReadableString))
    factorsAfterElimination
  }
  
  /* 
   * Determine if the second row can be absorbed into the first. This is true iff, for each position, either the second value is *
   * or they are equal.
   */
  private  def pairConsistent(range: Array[Extended[_]], index1: Int, index2: Int): Boolean = {
    val x1 = range(index1)
    val x2 = range(index2)
    x1 == x2 || !x2.isRegular
  }

  private def consistent(ranges: List[Array[Extended[_]]], indices1: List[Int], indices2: List[Int]): Boolean = {
    (ranges.zip(indices1).zip(indices2)).forall{ case ((range, indices1), indices2) => pairConsistent(range, indices1, indices2) }
  }

  /*
   * We use the mathematician's solution. Finalizing with no bounds is the same as finalizing with equal lower and upper bounds,
   * so we just delegate to normalizeAndAbsorbWithBounds.
   */
  private def normalizeAndAbsorbNoBounds(factor: Factor[Double]): Factor[(Double, Double)] = {
    normalizeAndAbsorbWithBounds(factor, factor)
  }
  
  /*
   * The job of normalizeAndAbsorbWithBounds is to take a factor consisting of unnormalized lower bounds for regular values and *,
   * and another factor containing unnormalized upper bounds for regular values and *, and to compute normalized lower and upper
   * bounds for each of the regular values. These bounds should satisfy that the true probability of the regular values lies
   * between the bounds.
   * 
   * The process of normalizeAndAbsorbWithBounds is as follows:
   * (1) Compute the sums of the unnormalized lower and upper bounds. These constitute lower and upper bounds on the normalizing factor,
   * respectively.
   * (2) Compute the normalized lower bounds, which are the unnormalized lower bounds divided by the upper bound on the normalizing factor.
   * The normalized lower bound on a regular value represents definitely allocated probability to that value, that cannot possibly be
   * allocated to any other regular value. This fact will be used in calculating one of the upper bounds.
   * (3) Compute the first upper bound. This consists of all the probability mass that has not definitely been allocated to other
   * values. Since the resulting factor might have more than one variable, we need to determine which rows probability mass can definitely
   * not be allocated to a particular row. This is determined by the consistent subroutine below. The first upper bound on the normalized 
   * probability of a given row is equal to 1 - the sum of normalized lower bounds of rows that are not consistent with this row.
   * (4) For the second upper bound, we use the opposite approach. We add together all the unnormalized upper bounds of rows that are
   * consistent with this row, and then divide by the lower bound on the normalizing factor.
   * (5) Finally, we set the lower and upper bounds of the row in the result to be the computed normalized lower bound and the lesser
   * of the two computed upper bounds.
   */
  private def normalizeAndAbsorbWithBounds(lowerFactor: Factor[Double], upperFactor: Factor[Double]): Factor[(Double, Double)] = {
    assert(lowerFactor.variables == upperFactor.variables)

    val ranges: List[Array[Extended[_]]] = lowerFactor.variables.map(_.range.toArray[Extended[_]])

    /*
     * First, we compute the sum of unnormalized lower and upper bounds.
     */
    val result = Factory.make[(Double, Double)](lowerFactor.variables)
    var lowerTotal = 0.0
    var upperTotal = 0.0
    var starTotal = 0.0
    val allIndicesIndexed = lowerFactor.allIndices.zipWithIndex
    for { (indices, i) <- allIndicesIndexed } {
      val lower = lowerFactor.get(indices)
      val upper = upperFactor.get(indices)
      lowerTotal += lower
      upperTotal += upper
    }

    /*
     * Get the definitely allocated probability mass: upperTotal is an upper bound on the normalizing factor.
     */ 
    var lowerBounds: Array[Double] = Array.fill(allIndicesIndexed.size)(0)
    for { (indices, i) <- allIndicesIndexed } { lowerBounds(i) = lowerFactor.get(indices) / upperTotal }
  
    /*
     * There are two possible upper bounds of a row.
     * The first is 1 - the definite lower bounds of incompatible rows.
     * The second is the sum of all compatible rows' unnormalized upper bounds
     * divided by the lower bound on the normalizing factor.
     * We use the lesser of these two bounds.
     */
    var upperBounds: Array[Double] = Array.fill(allIndicesIndexed.size)(0)
    for { (indices, i) <- allIndicesIndexed } {
      var inconsistentLowerTotal = 0.0
      var consistentUpperTotal = 0.0
      for { (otherIndices, j) <- allIndicesIndexed } {
        if (!consistent(ranges, indices, otherIndices)) inconsistentLowerTotal += lowerBounds(j)
        else consistentUpperTotal += upperFactor.get(otherIndices)
      }
      upperBounds(i) = (1.0 - inconsistentLowerTotal).min(consistentUpperTotal / lowerTotal)
    }
    
    /*
     * Finally, create the result factor with lower and upper bounds.
     */
    for { (indices, i) <- allIndicesIndexed } {
      result.set(indices, (lowerBounds(i), upperBounds(i)))
    }
    result
  }

  var targetFactors: Map[Element[_], Factor[(Double, Double)]] = Map()
  
  private def marginalizeToTarget(factor: Factor[(Double, Double)], target: Element[_]): Unit = {
    val targetFactor = factor.marginalizeTo(BoundsSumProductSemiring, Variable(target))
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[(Double, Double)]) = {
    targetElements foreach (marginalizeToTarget(resultFactor, _))
  }
    
  def probabilityBounds[T](target: Element[_], value: T): (Double, Double) = {
    require(targetElements contains target)
    val index = Variable(target).range.indexOf(Regular(value))
    if (index == -1) (0, 1) 
    else targetFactors(target).get(List(index))
  }

  def finishNoBounds(factorsAfterElimination: Set[Factor[Double]]): Factor[(Double, Double)] = {
    // It is possible that there are no factors (this will happen if there is no evidence).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    val multiplied = factorsAfterElimination.foldLeft(Factor.unit(semiring))(_.product(_, semiring))
    val normalized = normalizeAndAbsorbNoBounds(multiplied)
    normalized
  }

  def finishWithBounds(lowerFactors: Set[Factor[Double]], upperFactors: Set[Factor[Double]]): Factor[(Double, Double)] = {
    // It is possible that there are no factors (this will happen if there is no evidence).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    val lowerMultiplied = lowerFactors.foldLeft(Factor.unit(semiring))(_.product(_, semiring))
    val upperMultiplied = upperFactors.foldLeft(Factor.unit(semiring))(_.product(_, semiring))
    val normalized = normalizeAndAbsorbWithBounds(lowerMultiplied, upperMultiplied)
    normalized
  }
  
  /* This code is copied straight from VariableElimination. It should be refactored to avoid duplication. */
   // The first element of FactorMap is the complete set of factors.
  // The second element maps variables to the factors mentioning that variable.
  private type FactorMap[T] = Map[Variable[_], Set[Factor[T]]]

  private def addFactor[T](factor: Factor[T], map: FactorMap[T]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, Set()) + factor))

  private def removeFactor[T](factor: Factor[T], map: FactorMap[T]): Unit =
    factor.variables foreach (v => map += v -> (map.getOrElse(v, Set()) - factor))

  private def initialFactorMap(factors: Traversable[Factor[Double]]): FactorMap[Double] = {
    val map: FactorMap[Double] = Map()
    factors foreach (addFactor(_, map))
    map
  }

  protected var recordingFactors: List[Factor[_]] = List()

  /**
   * Some variable elimination algorithms, such as computing the most probable explanation, record values of
   * variables as they are eliminated. Such values are stored in a factor that maps values of the other variables
   * to a value of the eliminated variable. This factor is produced by finding the value of the variable that
   * "maximizes" the entry associated with the value in the product factor resulting from eliminating this
   * variable, for some maximization function. The recordingFunction determines which of two entries is greater
   * according to the maximization function. It returns true iff the second entry is greater. The recording
   * function is an option so that variable elimination algorithms that do not use it can ignore it.
   */
  val comparator: Option[(Double, Double) => Boolean] = None

  private def eliminate(
    variable: Variable[_],
    factors: Set[Factor[Double]],
    map: FactorMap[Double]): Unit = {
    val varFactors: Set[Factor[Double]] = map(variable)
    if (debug) {
      println("*****************\nEliminating " + variable.id)
      println("Input factors:")
      for { factor <- varFactors } { println(factor.toReadableString) }
    }
    if (varFactors.nonEmpty) {
      def multiply(f1: Factor[Double], f2: Factor[Double]): Factor[Double] = f1.product(f2, semiring)
      val productFactor: Factor[Double] = varFactors.reduceLeft(_.product(_, semiring))
      val resultFactor: Factor[Double] = productFactor.sumOver(variable, semiring)
      varFactors foreach (removeFactor(_, map))
      addFactor(resultFactor, map)
      comparator match {
        case None => ()
        case Some(recorder) => recordingFactors ::= productFactor.recordArgMax(variable, recorder)
      }
      map -= variable
      factors --= varFactors
      if (debug) println("Result factor\n" + resultFactor.toReadableString)
      factors += resultFactor
    }
  }

  private def eliminateInOrder(
    order: List[Variable[_]],
    factors: Set[Factor[Double]],
    map: FactorMap[Double]): Set[Factor[Double]] =
    order match {
      case Nil =>
        factors
      case first :: rest =>
        eliminate(first, factors, map)
        eliminateInOrder(rest, factors, map)
    }

  // for debugging
  private def printVariables(variables: Traversable[/*Extended*/Variable[_]]) {
    for { variable <- variables } {
      print("  ")
      variable match {
        case ev: ElementVariable[_] =>
          println(ev.element.toNameString)
        case _ => println(variable)
      }
    }
  }
  /**
   * Method for choosing the elimination order.
   * The default order chooses first the variable that
   * minimizes the number of extra factor entries that would be created when it is eliminated.
   * Override this method if you want a different rule.
   */
  def eliminationOrder(allVars: Set[Variable[_]], factors: Traversable[Factor[Double]], toPreserve: Traversable[Variable[_]]): List[Variable[_]] = {
    val eliminableVars =  allVars -- toPreserve
    var initialGraph = new VEGraph(factors)
    val candidates = new HeapPriorityMap[Variable[_], Double]
    eliminableVars foreach (v => candidates += v.asInstanceOf[Variable[_]] -> initialGraph.score(v))
    eliminationOrderHelper(candidates, toPreserve, initialGraph, List())
  }

  @tailrec private def eliminationOrderHelper(candidates: PriorityMap[Variable[_], Double],
    toPreserve: Traversable[Variable[_]],
    graph: VEGraph,
    accum: List[Variable[_]]): List[Variable[_]] = {
    if (candidates.isEmpty) accum.reverse
    else {
      val best = candidates.extractMin()._1
      // do not read the best variable after it has been removed, and do not add the preserved variables
      val touched = graph.info(best).neighbors - best -- toPreserve
      val nextGraph = graph.eliminate(best)
      touched foreach (v => candidates += v.asInstanceOf[Variable[_]] -> graph.score(v))
      eliminationOrderHelper(candidates, toPreserve, nextGraph, best :: accum)
    }
  }

}
