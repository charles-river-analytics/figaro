/*
 * LikelihoodWeighter.scala
 * Likelihood weighting works by propagating observations through Dists and Chains to the variables they depend on.
 *
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.sampling

import com.cra.figaro.language._
import scala.annotation.tailrec
import com.cra.figaro.library.cache.Cache
import scala.collection.mutable.Set

/*
 *  Likelihood weighting works by propagating observations through Dists and Chains
 *  to the variables they depend on. If we don't make sure we sample those Dists and
 *  Chains first, we may end up sampling those other elements without the correct
 *  observations. To avoid this, we keep track of all these dependencies.
 *  The dependencies map contains all the elements that could propagate an
 *  observation to any given element.
 *  To avoid calling values on Chains, we dynamically build the list of dependencies. 
 *  If we encounter the result element of a chain before it's parent, we redo the sampling
 *  on the result and undo any weight associated with that element. 
 *   
 */

/**
 * A class that implements sampling via likelihood weighting on a set of elements.
 */
class LikelihoodWeighter(universe: Universe, cache: Cache) {

  /* Stores the dependencies between elements for likelihood weighting */
  private[figaro] val dependencies = scala.collection.mutable.Map[Element[_], Set[Element[_]]]()
  universe.register(dependencies)

  /**
   * Clear the cache
   */
  def clearCache() = {
    cache.clear
  }

  /** 
   * Deregister the map of dependencies between elements
   */  
   def deregisterDependencies() = {
    universe.deregister(dependencies)
  }

  /**
   * Sample each element in the list of elements and compute their likelihood weight
   */
  def computeWeight(elementsToVisit: List[Element[_]]): Double = {
    // Do all dependencies first, then no need to check for them in the main traversal loop
    val visited: Set[Element[_]] = Set()
    val dependencyWeights = traverse(List(), dependencies.values.flatten.toList, 0.0, visited)
    val remaining = elementsToVisit.filterNot(visited.contains(_))
    traverse(List(), remaining, dependencyWeights, visited)
  }

  /*
   * Traverse the elements in generative order, and return the weight
   */
  @tailrec
  private[figaro] final def traverse(currentStack: List[(Element[_], Option[_], Option[Element[_]])],
    elementsToVisit: List[Element[_]], currentWeight: Double, visited: Set[Element[_]]): Double = {

    // If everything is empty, just return the weight
    if (elementsToVisit.isEmpty && currentStack.isEmpty) {
      currentWeight

    } // If the current stack is empty, take the head of the elements to visit as the next element
    else if (currentStack.isEmpty) {
      traverse(List((elementsToVisit.head, getObservation(elementsToVisit.head, None), None)), elementsToVisit.tail, currentWeight, visited)

    } // If the head of the stack has already been visited, is not active, or is in another universe, we don't need to do anything, go to the next element
    else if (!currentStack.head._1.active || visited.contains(currentStack.head._1) || currentStack.head._1.universe != universe) {
      traverse(currentStack.tail, elementsToVisit, currentWeight, visited += currentStack.head._1)

    } // Otherwise, we need to process the top element on the stack
    else {
      val (currElem, currObs, currResult) = currentStack.head

      currElem match {
        case dist: Dist[_, _] =>
          val parents = dist match {
            case dc: CompoundDist[_] => dc.probs.filterNot(visited.contains(_)).map(e => (e, getObservation(e, None), None))
            case _ => List()
          }

          if (parents.nonEmpty) {
            traverse(parents ::: currentStack, elementsToVisit, currentWeight, visited)
          } else {
            val rand = dist.generateRandomness()
            val index = dist.selectIndex(rand)
            val resultElement = if (currResult.isEmpty) dist.outcomeArray(index) else currResult.get
            val nextHead = List((resultElement, getObservation(resultElement, currObs), None), (currElem, None, Some(resultElement)))

            if (visited.contains(resultElement) && currObs.nonEmpty) {
              // we did this in the wrong order, and have to repropagate the result for likelihood weighting, and add it to the dependency map so we don't do this incorrectly next time              
              val elementsToRedo = findDependentElements(dist, resultElement)
              val newWeight = (currentWeight /: elementsToRedo)((c: Double, n: Element[_]) => undoWeight(c, n))
              traverse(nextHead ::: currentStack.tail, elementsToRedo.toList ::: elementsToVisit, newWeight, visited --= elementsToRedo)
            } else if (!visited.contains(resultElement)) {
              traverse(nextHead ::: currentStack.tail, elementsToVisit, currentWeight, visited)
            } else {
              dist.value = resultElement.value.asInstanceOf[dist.Value]
              dependencies.getOrElseUpdate(resultElement, Set()) += dist
              val nextWeight = computeNextWeight(currentWeight, currElem, currObs)
              traverse(currentStack.tail, elementsToVisit, nextWeight, visited += currElem)
            }
          }

        case chain: Chain[_, _] =>
          if (!visited.contains(chain.parent)) {
            traverse((chain.parent, getObservation(chain.parent, None), None) :: currentStack, elementsToVisit, currentWeight, visited)
          } else {
            val next = if (currResult.isEmpty) cache(chain).get else currResult.get
            val nextHead = List((next, getObservation(next, currObs), None), (currElem, None, Some(next)))
            if (visited.contains(next) && currObs.nonEmpty) {
              // we did this in the wrong order, and have to repropagate the result for likelihood weighting
              val elementsToRedo = findDependentElements(chain, next)
              val newWeight = (currentWeight /: elementsToRedo)((c: Double, n: Element[_]) => undoWeight(c, n))
              traverse(nextHead ::: currentStack.tail, elementsToRedo.toList ::: elementsToVisit, newWeight, visited --= elementsToRedo)
            } else if (!visited.contains(next)) {
              traverse(nextHead ::: currentStack.tail, elementsToVisit, currentWeight, visited)
            } else {
              chain match {
                case cc: CachingChain[_, _] => if (!next.isTemporary) dependencies.getOrElseUpdate(next, Set()) += chain
                case _ => ()
              }
              chain.value = next.value.asInstanceOf[chain.Value]
              val nextWeight = computeNextWeight(currentWeight, currElem, currObs)
              traverse(currentStack.tail, elementsToVisit, nextWeight, visited += currElem)
            }
          }
        case _ =>
          val args = (currElem.args ::: currElem.elementsIAmContingentOn.toList)
          // Find all the arguments of the element that have not been visited
          val remainingArgs = args.filterNot(visited.contains(_)).map(e => (e, getObservation(e, None), None))
          // if there are args unvisited, push those args to the top of the stack            
          if (remainingArgs.nonEmpty) {
            traverse(remainingArgs ::: currentStack, elementsToVisit, currentWeight, visited)
          } else {
            // else, we can now process this element and move on to the next item
            currElem.randomness = currElem.generateRandomness()
            currElem.value = currElem.generateValue(currElem.randomness)
            val nextWeight = computeNextWeight(currentWeight, currElem, currObs)
            traverse(currentStack.tail, elementsToVisit, nextWeight, visited += currElem)
          }
      }
    }
  }

  
  /*
   * Finds the set of elements that need to be resampled when the likelihood weighting went in the wrong order
   */
  private def findDependentElements(elem: Element[_], result: Element[_]) = {
    val chainUsedBy = universe.usedBy(elem) + elem
    val resultUseBy = universe.usedBy(result) + result
    resultUseBy -- chainUsedBy
  }

  /*
   * Get the observation on an element, merging with any propagated observation from likelihood weighting
   */
  protected def getObservation(element: Element[_], observation: Option[_]): Option[Any] = {
    (observation, element.observation) match {
      case (None, None) => None
      case (Some(obs), None) => Some(obs)
      case (None, Some(obs)) => Some(obs)
      case (Some(obs1), Some(obs2)) if obs1 == obs2 => Some(obs1)
      case _ => { // incompatible observations
        rejectionAction()
        None
      }
    }
  }

  /*
   * Compute the current weight of the model by incorporating the weight of the current sampled element
   * If there is no observation on the element, the weight is the current weight plus the constraint on the element.
   * If there is a condition on the element (not an observation), then we throw a rejection if the condition is not met.
   * 
   * If there is an observation on the element, we implement likelihood weighting. If the element has a density
   * function, we add the log density to the current weight. If it doesn't have a density, we check to see if 
   * it satisfies the observation
   */
  private[figaro] def computeNextWeight(currentWeight: Double, element: Element[_], obs: Option[_]): Double = {
    val nextWeight = if (obs.isEmpty) {
      if (!element.condition(element.value)) rejectionAction()
      currentWeight
    } else {
      element match {
        case f: CompoundFlip => {
          setObservation(element, obs)
          if (obs.get.asInstanceOf[Boolean]) currentWeight + math.log(f.prob.value)
          else currentWeight + math.log(1 - f.prob.value)
        }
        case e: HasDensity[_] => {
          setObservation(element, obs)
          val density = element.asInstanceOf[HasDensity[element.Value]].density(obs.asInstanceOf[Option[element.Value]].get)
          currentWeight + math.log(density)
        }
        case _ => {
          if (!element.condition(element.value)) rejectionAction()
          currentWeight
        }
      }
    }
    nextWeight + element.constraint(element.value)
  }

  protected def setObservation(element: Element[_], obs: Option[_]) = element.value = obs.get.asInstanceOf[element.Value]  
  
  /* Action to take on a rejection. By default it throws an Importance.Reject exception, but this can be overriden for another behavior */
  protected def rejectionAction(): Unit = throw Importance.Reject

  /*  
   * Undo the application of this elements weight if we did likelihood weighting in the wrong order
   */
  private def undoWeight(weight: Double, elem: Element[_]) = weight - computeNextWeight(0.0, elem, elem.observation)

}


