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
import com.cra.figaro.library.cache.Cache
import com.cra.figaro.library.cache.NoCache
import com.cra.figaro.algorithm.sampling.LikelihoodWeighter


class ForwardWeighter(universe: Universe, cache: Cache) extends LikelihoodWeighter(universe, cache) {
  override def rejectionAction() = ()
  override def setObservation(element: Element[_], obs: Option[_]) = {}
}

/**
 * A forward sampler that generates a state by generating values for elements, making sure to generate all the
 * arguments of an element before the element.
 */
object Forward {
  /**
   * Sample the universe by generating a value for each element of the universe. Return a cache object.
   */
  def apply(universe: Universe): Double = {
    apply(universe, new NoCache(universe))
  }

  /**
   * Sample the universe by generating a value for each element of the universe, and provide a cache object. Return a cache object.
   */
  def apply(universe: Universe, cache: Cache): Double = {
    val lw = new ForwardWeighter(universe, cache)
    try {
      lw.computeWeight(universe.activeElements)
    } catch {
      case Importance.Reject => Double.NegativeInfinity 
    }    
  }

  /**
   * Sample only part of the model originating from a single element
   */
  def apply[T](element: Element[T]): Double = {
    val noCache = new NoCache(element.universe)
    val lw = new ForwardWeighter(element.universe, noCache)
    val weight = try {
      lw.computeWeight(List(element))
    } catch {
      case Importance.Reject => Double.NegativeInfinity 
    }
    element.universe.deregister(noCache)
    weight
  }
  
}
