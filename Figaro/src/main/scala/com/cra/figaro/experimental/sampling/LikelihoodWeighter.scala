package com.cra.figaro.experimental.sampling

import com.cra.figaro.language._
import scala.annotation.tailrec
import com.cra.figaro.algorithm.sampling.Importance

class LikelihoodWeighter(universe: Universe) {

  def computeWeight(elementsToVisit: Set[Element[_]]): Double = {
    traverse(List(), elementsToVisit, 0.0, Set(), scala.collection.mutable.Map[Dist[_, _], Int]())
  }
 
  @tailrec
  private final def traverse(currentStack: List[(Element[_], Option[_])],
    elementsToVisit: Set[Element[_]],
    currentWeight: Double,
    visited: Set[Element[_]], DistMap: scala.collection.mutable.Map[Dist[_, _], Int]): Double = {

    // If everything is empty, just return the weight
    if (elementsToVisit.isEmpty && currentStack.isEmpty) {
      currentWeight
    } 
    // If the current stack is empty, we are free to choose any element to traverse. Pick the head of the set
    else if (currentStack.isEmpty) {
      traverse(List((elementsToVisit.head, getObservation(elementsToVisit.head, None))), elementsToVisit.tail, currentWeight, visited, DistMap)
    } 
    // If the head of the stack has already been visited or in another universe, we don't need to do anything, go to the next element
    else if (visited.contains(currentStack.head._1) || currentStack.head._1.universe != universe) {
      traverse(currentStack.tail, elementsToVisit, currentWeight, visited, DistMap)
    } 
    // Otherwise, we need to process the top element on the stack
    else {
      val (currElem, currObs) = currentStack.head      

      currElem match {
        case d: Dist[_, _] =>
          val parents = d match {
            case dc: CompoundDist[_] => dc.probs.filterNot(visited.contains(_)).map(e => (e,  getObservation(e, None)))
            case _ => List()
          }
          val rand = d.generateRandomness()
          val index = d.selectIndex(rand)
          val resultElement = d.outcomeArray(index)
          val nextHead = List((resultElement, getObservation(resultElement, currObs)), (currElem, None))
          
          if (parents.nonEmpty) {
            traverse(parents ::: currentStack, elementsToVisit, currentWeight, visited, DistMap)
          } else if (visited.contains(resultElement) && currObs.nonEmpty) {                        
            traverse(nextHead ::: currentStack.tail, elementsToVisit, undoWeight(currentWeight, resultElement), visited - resultElement, DistMap += (d -> index))
          } else if (!visited.contains(resultElement)) {
            traverse(nextHead ::: currentStack.tail, elementsToVisit, currentWeight, visited, DistMap += (d -> index))
          } else {
            d.value = if (DistMap.contains(d)) d.finishGeneration(DistMap(d)) else d.finishGeneration(index)
            DistMap -= d
            val nextWeight = computeNextWeight(currentWeight, currElem, currObs)
            traverse(currentStack.tail, elementsToVisit - currElem, nextWeight, visited + currElem, DistMap)
          }
        case c: Chain[_, _] =>
          if (!visited.contains(c.parent)) {
            traverse((c.parent,  getObservation(c.parent, None)) +: currentStack, elementsToVisit, currentWeight, visited, DistMap)
          } else {
            val next = c.get(c.parent.value)
            val nextHead = List((next, getObservation(next, currObs)), (currElem, None))
            if (visited.contains(next) && currObs.nonEmpty) {
              // we did this in the wrong order, and have to repropagate the result for likelihood weighting                            
              traverse(nextHead ::: currentStack.tail, elementsToVisit, undoWeight(currentWeight, next), visited - next, DistMap)
            } else if (!visited.contains(next)) {              
              traverse(nextHead ::: currentStack.tail, elementsToVisit, currentWeight, visited, DistMap)
            } else {
              c.value = next.value
              val nextWeight = computeNextWeight(currentWeight, currElem, currObs)
              traverse(currentStack.tail, elementsToVisit - currElem, nextWeight, visited + currElem, DistMap)
            }
          }
        case _ =>
          val args = (currElem.args ::: currElem.elementsIAmContingentOn.toList)
          // Find all the arguments of the element that have not been visited
          val remainingArgs = args.filterNot(visited.contains(_)).map(e => (e, getObservation(e, None)))
          // if there are args unvisited, push those args to the top of the stack            
          if (remainingArgs.nonEmpty) {
            traverse(remainingArgs ::: currentStack, elementsToVisit, currentWeight, visited, DistMap)
          } else {
            // else, we can now process this element and move on to the next item
            currElem.randomness = currElem.generateRandomness()
            currElem.value = currElem.generateValue(currElem.randomness)
            val nextWeight = computeNextWeight(currentWeight, currElem, currObs)
            traverse(currentStack.tail, elementsToVisit - currElem, nextWeight, visited + currElem, DistMap)
          }
      }

    }
  }

  def getObservation(element: Element[_], observation: Option[_]) = {
    (observation, element.observation) match {
      case (None, None) => None
      case (Some(obs), None) => Some(obs)
      case (None, Some(obs)) => Some(obs)
      case (Some(obs1), Some(obs2)) if obs1 == obs2 => Some(obs1)
      case _ => throw Importance.Reject // incompatible observations
    }
  }  

  def computeNextWeight(currentWeight: Double, element: Element[_], obs: Option[_]): Double = {
    val nextWeight = if (obs.isEmpty) {
      if (!element.condition(element.value)) throw Importance.Reject
      currentWeight
    } else {      
      element match {
        case f: CompoundFlip => {
          element.value = obs.get.asInstanceOf[element.Value]
          if (obs.get.asInstanceOf[Boolean]) currentWeight + math.log(f.prob.value)
          else currentWeight + math.log(1 - f.prob.value)
        }   
        case e: HasDensity[_] => {         
          element.value = obs.get.asInstanceOf[element.Value]
          val density = element.asInstanceOf[HasDensity[element.Value]].density(obs.asInstanceOf[Option[element.Value]].get)
          currentWeight + math.log(density)
        }
        case _ => {
          if (!element.condition(element.value)) throw Importance.Reject
          currentWeight
        }
      }
    }
    nextWeight + element.constraint(element.value)
  }
  
  def undoWeight(weight: Double, elem: Element[_]) = weight - computeNextWeight(0.0, elem, elem.observation)

}


