/*
 * Forward.scala
 * Forward sampling.
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

/**
 * A forward sampler that generates a state by generating values for elements, making sure to generate all the
 * arguments of an element before the element.
 */
object Forward {
  /**
   * Sample the universe by generating a value for each element of the universe.
   */
  def apply(implicit universe: Universe) = {
    // avoiding recursion
    var state = Set[Element[_]]()
    var elementsRemaining = universe.activeElements
    while (!elementsRemaining.isEmpty) {
      if (elementsRemaining.head.active) state = sampleInState(elementsRemaining.head, state, universe)
      elementsRemaining = elementsRemaining.tail
    }
  }
  
  def apply[T](element: Element[T]) = {
    sampleInState(element, Set[Element[_]](), element.universe)
  }

  private type State = Set[Element[_]]

  /*
   * To allow this algorithm to be used for dependent universes, we make sure elements in a different universe are not
   * sampled.
   */
  private def sampleInState[T](element: Element[T], state: State, universe: Universe): State = {
    if (element.universe != universe || (state contains element)) state
    else {
      val (state1, sampledValue) = {
        element match {
          case d: Dist[_, _] =>
            val state1 =
              d match {
                case dc: CompoundDist[_] =>
                  // avoiding recursion
                  var resultState = state
                  var probsRemaining = dc.probs
                  while (!probsRemaining.isEmpty) {
                    resultState = sampleInState(probsRemaining.head, resultState, universe)
                    probsRemaining = probsRemaining.tail
                  }
                  resultState
                case _ => state
              }
            val rand = d.generateRandomness()
            val index = d.selectIndex(rand)
            val state2 = sampleInState(d.outcomeArray(index), state1, universe)
            (state2, d.finishGeneration(index))
          case c: Chain[_, _] =>
            val state1 = sampleInState(c.parent, state, universe)
            val result = c.get(c.parent.value)
            val state2 = sampleInState(result, state1, universe)
            (state2, result.value)
          case _ =>
            // avoiding recursion
            var state1 = state
            var argsRemaining = (element.args ::: element.elementsIAmContingentOn.toList)
            while (!argsRemaining.isEmpty) {
              state1 = sampleInState(argsRemaining.head, state1, universe)
              argsRemaining = argsRemaining.tail
            }
            element.randomness = element.generateRandomness
            (state1, element.generateValue(element.randomness))
        }
      }
      element.value = sampledValue.asInstanceOf[T]
      state1 + element
    }
  }
}
