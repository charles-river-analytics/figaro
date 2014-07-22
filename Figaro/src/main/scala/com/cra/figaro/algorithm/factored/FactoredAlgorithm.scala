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
import scala.collection.immutable.Set
import com.cra.figaro.algorithm.UnsupportedAlgorithmException

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
    // Since there may be evidence on the dependent universes, we have to include their parents as important elements
    val dependentUniverseParents = 
      for { 
        (dependentUniverse, _) <- dependentUniverses.toList
        parentElement <- dependentUniverse.parentElements
        if parentElement.universe == universe
      } yield parentElement
    // Any element on which an element with a condition or constraint is contingent is also important
    val boundsInducingElements = universe.conditionedElements ::: universe.constrainedElements ::: dependentUniverseParents

    // Later, we will need to draw connections between elements that are both parents of the same dependent universe
    var dependentUniverseCoparents: Map[Element[_], Set[Element[_]]] = Map()
    for {
      (dependentUniverse, _) <- dependentUniverses
      parent <- dependentUniverse.parentElements
    } {
      val previous = dependentUniverseCoparents.getOrElse(parent, Set())
      dependentUniverseCoparents += parent -> (previous ++ dependentUniverse.parentElements.toSet - parent)
    }
    // Make sure we compute values from scratch in case the elements have changed
    LazyValues.clear(universe)
    val values = LazyValues(universe)

    /* 
     * Beginning with the given element at the given depth, find all elements that the given element is used by within the depth.
     * Also chases down other elements that are coparents of the same dependent universe.
     * Each time we go down to the next element, the depth is reduced by 1.
     * An element is included in the result if its required depth is greater than its previously expanded depth.
     */
    def chaseDown(element: Element[_], depth: Int, chasedSoFar: Set[Element[_]]): Set[(Element[_], Int)] = {
        if (depth >= 0) {
          val includeThisElement = depth > LazyValues(element.universe).expandedDepth(element).getOrElse(-1)
          // Keeping track of what's been chased so far avoids infinite recursion
          val toChase = element.universe.directlyUsedBy(element).toSet ++ dependentUniverseCoparents.getOrElse(element, Set()) -- chasedSoFar
          val rest = toChase.flatMap((elem: Element[_]) => chaseDown(elem, depth - 1, chasedSoFar ++ toChase))
          if (includeThisElement) rest + ((element, depth)); else rest
        } else Set()
      }
      
    /* 
     * We start with the starter elements (typically the queries), because we don't need to consider elements that are unreachable from them. 
     * For any variable that is expanded, we need to make sure that a variable that it is contingent on is also expanded.
     * 
     * */
    val newlyNeededElements = 
      Element.closeUnderContingencies(starterElements.toSet).map((elem: Element[_]) => (elem, depth))
    
    @tailrec
    def expandElements(curr: Set[(Element[_], Int)]): Unit = {
      if(curr.isEmpty) return
      val currGrouped = curr.groupBy(_._1.universe)
      val allNeededElements = currGrouped.flatMap((pair: (Universe, Set[(Element[_], Int)])) => {
        val (uni, set) = pair
        val values = LazyValues(uni)
        values.expandAll(set)
        val currentlyExpanded = values.expandedElements.toSet
        val currentDepths = currentlyExpanded.map(d => (d, values.expandedDepth(d).getOrElse(0)))
        val others = currentDepths.flatMap(e => chaseDown(e._1, e._2, currentlyExpanded))
        val filteredElements = others.filter(o => boundsInducingElements.contains(o._1))
        val neededElements = filteredElements.flatMap(f => (f._1.elementsIAmContingentOn + f._1).map((_, f._2)))
        neededElements
      })
      expandElements(allNeededElements.toSet)
    }
    
    expandElements(newlyNeededElements.toSet)
    
    // We only include elements from other universes if they are specified in the starter elements.
    val resultElements = values.expandedElements.toList ::: starterElements.filter(_.universe != universe)
    
    resultElements.foreach(p => p match {
      case n: NonCachingChain[_,_] => throw new UnsupportedAlgorithmException(n)
      case _ => 
    })
        
    // Only conditions and constraints produce distinct lower and upper bounds for *. So, if there are not such elements with * as one
    // of their values, we don't need to compute bounds and can save computation.
    val boundsNeeded = boundsInducingElements.exists(e => LazyValues(e.universe).storedValues(e).hasStar)
    // We make sure to clear the variable cache now, once all values have been computed.
    // This ensures that any created variables will be consistent with the computed values.
    Variable.clearCache()
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





