/*
 * TopSort.scala
 * 
 * Created By:      Alison O'Connor (aoconnor@cra.com)
 * Creation Date:   August 15, 2017
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.util

import scala.collection.mutable.ListBuffer

import com.cra.figaro.language.Apply
import com.cra.figaro.language.Chain
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Element
import com.cra.figaro.language.Flip
import com.cra.figaro.language.Name.stringToName
import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.If

/**
 * @author aoconnor
 */
object TopSort {

  /* If debug, then print out intermediate and final solutions */
  var debug = false

  /* Returns the list of elements that directly used an element that are in the list of possible elements */
  private def topSortDirectlyUses(universe: Universe, element: Element[_], possibleElements: List[Element[_]]) = {
    universe.directlyUses(element).filter(elem => possibleElements contains elem)
  }

  /* Returns a list of elements sorted by conditioned elements, then constrained elements, then other elements */
  private def prioritySort(universe: Universe, elements: List[Element[_]]) = {
    val priorityElements = (universe.conditionedElements intersect elements) ++ (universe.constrainedElements intersect elements)
    val otherElements = elements diff priorityElements
    priorityElements ++ otherElements
  }

  /* Returns a topologically sorted list of elements in the universe, given an unsorted list of elements */
  def topologicallySort(updatesNeeded: Iterable[Element[_]], universe: Universe) = {

    //Create a list of the original updates needed and all their recursive elements
    val elements = updatesNeeded.toList
    if (debug) { println("No. recursive elements: " + elements.size); elements foreach (e => print(e.name + ", ")); println }

    //Compute the in-degree (number of incoming edges) for each of the elements
    var indegree = (elements zip elements.map(elem => topSortDirectlyUses(universe, elem, elements).size)).toMap

    //Initialize the count of visited nodes as 0
    var visitedNodes = 0

    //Initialize the topological ordering 
    var topologicalOrder: ListBuffer[Element[_]] = ListBuffer()

    //Start with the elements with in-degree of 0 and add them into a queue
    val noParents = prioritySort(universe, indegree.filter(entry => (entry._2 == 0)).map(entry => entry._1).toList)
    var queue: ListBuffer[Element[_]] = ListBuffer(noParents: _*)

    //While the queue is not empty
    while (!queue.isEmpty) {

      //Increment count of visited nodes by 1
      visitedNodes += 1

      //Get the head of the queue, remove it from the queue, and add it to the topological order
      val head = queue.head
      queue -= head
      topologicalOrder += head

      //Decrease in-degree by 1 for all its neighboring nodes
      var addToQueue: ListBuffer[Element[_]] = ListBuffer()
      for (neighbor <- universe.directlyUsedBy(head)) {
        val newDegree = (indegree(neighbor) - 1)
        indegree += (neighbor -> (newDegree))
        //If in-degree of a neighboring nodes is reduced to zero, then add it to the queue
        if (newDegree == 0) addToQueue += neighbor
      }
      queue ++= prioritySort(universe, addToQueue.toList)

    }

    //If count of visited nodes is not equal to the number of nodes in the graph then the topological sort is not possible for the given graph
    if (visitedNodes != elements.size) throw new Exception("No topological sorting is possible.")

    if (debug) { println("Top. order:"); topologicalOrder.foreach(elem => println(elem.name + ", ")) }

    topologicalOrder.toList
  }

}