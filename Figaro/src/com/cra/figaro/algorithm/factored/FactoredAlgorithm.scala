/*
 * FactoredAlgorithm.scala
 * Trait for algorithms that use factors
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jul 1, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import annotation.tailrec
import scala.collection._
import com.cra.figaro.algorithm.lazyfactored._
import scala.collection.mutable.Set

/**
 * Trait for algorithms that use factors.
 */
trait FactoredAlgorithm[T] extends Algorithm {
    
  /**
   * Get the elements that are needed by the query target variables and the evidence variables. Also compute the values
   * of those variables to the given depth. 
   * Only get factors for elements that are actually used by the target variables. This is more efficient.
   * Also, it avoids problems when values of unused elements have not been computed.
   * 
   * In addition to getting all the needed elements, it determines if any of the conditioned, constrained, or dependent universe parent elements has * in its range.
   * If any of these elements has * in its range, the lower and upper bounds of factors will be different, so we need to compute both.
   * If they don't, we don't need to compute bounds. 
   */
  def getNeededElements(starterElements: List[Element[_]], depth: Int): (List[Element[_]], Boolean) = {
    // Make sure we compute values from scratch in case the elements have changed
    LazyValues.clear(universe)

    // Since there may be evidence on the dependent universes, we have to include their parents as important elements
    val dependentUniverseParents = 
      for { 
        (dependentUniverse, _) <- dependentUniverses.toList
        parentElement <- dependentUniverse.parentElements
        if parentElement.universe == universe
      } yield parentElement
    val boundsInducingElements = universe.conditionedElements ::: universe.constrainedElements ::: dependentUniverseParents
    val preImportantElements = starterElements ::: boundsInducingElements 
    // Any element on which an important element is contingent is also important
    val importantElements = Element.closeUnderContingencies(Set(preImportantElements:_*))
    val values = LazyValues(universe)
    values.expandAll(importantElements.toSet, depth)
    // We need to create factors for any elements that are used by the important elements.
    // However, if an element has not been explored (its value set contains only Star), we do not create factors for it,
    // because no value of this variable can contribute positive probability to the query.
    // If we did create a factor, we might ultimately obtain that its value Star has zero probability, which would lead to
    // incorrect zero probability for the entire query.
    /*
    val usedAndExploredElements =
      for {
        important <- importantElements
        used <- universe.uses(important)
        if !LazyValues(universe).storedValues(used).values.isEmpty
      } yield used
    val resultElements = (usedAndExploredElements ++ importantElements).toList
    */
    val resultElements = values.expandedElements.toList
    val boundsNeeded = boundsInducingElements.exists(LazyValues(universe).storedValues(_).hasStar)
    
    (resultElements, boundsNeeded)
  }
  
  /**
   * All implementations of factored algorithms must specify a way to get the factors from the given universe and
   * dependent universes.
   */
  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upperBounds: Boolean = false): List[Factor[T]]
  
  /**
   * The universe on which this variable elimination algorithm should be applied.
   */
  val universe: Universe
  
  /**
   * A list of universes that depend on this universe such that evidence on those universes should be taken into
   * account in this universe.
   */
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])]

  /**
   * The algorithm to compute probability of specified evidence in a dependent universe.
   * We use () => Double to represent this algorithm instead of an instance of ProbEvidenceAlgorithm. 
   * Typical usage is to return the result of ProbEvidenceAlgorithm.computeProbEvidence when invoked.
   */
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double

  /**
   * The sum, product operations on the factor types and
   * appropriate values for zero and one must be defined.
   */
  val semiring: Semiring[T]
}





