/*
 * Importance.scala
 * Importance sampler.
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

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import scala.annotation.tailrec
import scala.collection.mutable.{ Set, Map }

/**
 * Importance samplers.
 */
abstract class Importance(universe: Universe, targets: Element[_]*)
  extends WeightedSampler(universe, targets: _*) {
  import Importance.State

  /*
   * Produce one weighted sample of the given element. weightedSample takes into account conditions and constraints
   * on all elements in the Universe, including those that depend on this element.
   */
  @tailrec final def sample(): Sample = {
    val resultOpt: Option[Sample] =
      try {
        val state = State()
        // We must make a fresh copy of the active elements since sampling can add active elements to the Universe      
        //val activeElements = universe.permanentElements
        // use active elements instead of permanent elements since permenentElements may not resample elements inside a chain
        val activeElements = universe.activeElements
        // Hack alert: Using reverse gets us to sample Dists before their choices, which is important for likelihood weighting
        // The algorithm is basically correct with and without reverse. This is an optimization.
        activeElements.reverse.foreach(e => if (e.active) sampleOne(state, e, None))
        val bindings = targets map (elem => elem -> elem.value)
        Some((state.weight, Map(bindings: _*)))
      } catch {
        case Importance.Reject =>
          None
      }

    resultOpt match {
      case Some(x) => x
      case None =>
        sample()
    }
  }

  /*
   * Sample the value of an element. If it has already been assigned in the state, the current assignment is
   * used and the state is unchanged. Also, an element in a different universe is not sampled; instead its state is
   * used directly
   *
   * This is made private[figaro] to allow easy testing
   */
  private[figaro] def sampleOne[T](state: State, element: Element[T], observation: Option[T]): T = {
    if (element.universe != universe || (state.assigned contains element)) element.value
    else {
      state.assigned += element
      sampleFresh(state, element, observation)
    }
  }

  /*
   * Sample a fresh value of an element, assuming it has not yet been assigned a value in the state. This sampling
   * takes into account the condition and constraint on the element. If the condition is violated, the entire sampling
   * process is rejected. This function returns the state including the new assignment to the element with the weight
   * of the constraint multiplied in.
   */
  private def sampleFresh[T](state: State, element: Element[T], observation: Option[T]): T = {
    val fullObservation = (observation, element.observation) match {
      case (None, None) => None
      case (Some(obs), None) => Some(obs)
      case (None, Some(obs)) => Some(obs)
      case (Some(obs1), Some(obs2)) if obs1 == obs2 => Some(obs1)
      case _ => throw Importance.Reject // incompatible observations
    }
    val value: T = 
      if (fullObservation.isEmpty || !element.isInstanceOf[Atomic[_]]) {
        val result = sampleValue(state, element, fullObservation)
        if (!element.condition(result)) throw Importance.Reject
        result
      } else {
        // Optimize the common case of an observation on an atomic element.
        // This partially implements likelihood weighting by clamping the element to its
        // desired value and multiplying the weight by the density of the value.
        // This can dramatically reduce the number of rejections.
        val obs = fullObservation.get
        state.weight += math.log(element.asInstanceOf[Atomic[T]].density(obs))
        obs
      }
    state.weight += element.constraint(value)
    value
  }

  /*
   * Sample the value of an element according to its generative model, without considering the condition or constraint.
   * Since sampling the value of this element might also involve sampling the values of related elements, the state
   * must be updated and returned.
   *
   * Most elements can simply be handled by sampling values for the arguments and generating values for this
   * element. Dist is an exception, because not all the outcomes need to be generated, but we only know which one
   * after we have sampled the randomness of the Dist. For this reason, we write special code to handle Dists.
   * For Chain, we also write special code to avoid calling get twice.
   * 
   * We propagate observations on Chains and Dists to their possible outcome elements. This ensures that instead of
   * sampling these elements and then checking whether the observation is satisfied, we set their values to the
   * required ones. This implements likelihood weighting and leads to faster convergence of the algorithm.
   */
  private def sampleValue[T](state: State, element: Element[T], observation: Option[T]): T = {
    element match {
      case d: Dist[_, _] =>
        d match {
          case dc: CompoundDist[_] => dc.probs foreach (sampleOne(state, _, None))
          case _ => ()
        }
        val rand = d.generateRandomness()
        val index = d.selectIndex(rand)
        sampleOne(state, d.outcomeArray(index), observation)
        d.value = d.finishGeneration(index)
        d.value
      case c: Chain[_, _] =>
        val parentValue = sampleOne(state, c.parent, None)
        c.value = sampleOne(state, c.get(parentValue), observation)
        c.value
      case f: CompoundFlip =>
        val probValue = sampleOne(state, f.prob, None)
        observation match {
          case Some(true) =>
            state.weight += math.log(probValue)
            true
          case Some(false) =>
            state.weight += math.log(1 - probValue)
            false
          case _ => 
            val result = random.nextDouble() < probValue
            f.value = result
            result
        }
      case f: ParameterizedFlip =>
        val probValue = sampleOne(state, f.parameter, None)
        observation match {
          case Some(true) =>
            state.weight += math.log(probValue)
            true
          case Some(false) =>
            state.weight += math.log(1 - probValue)
            false
          case _ => 
            val result = random.nextDouble() < probValue
            f.value = result
            result
        }
      case _ =>
        (element.args ::: element.elementsIAmContingentOn.toList) foreach (sampleOne(state, _, None))
        element.randomness = element.generateRandomness()
        element.value = element.generateValue(element.randomness)
        element.value
    }
  }
}

object Importance {
  /*
   * An element cannot be assigned more than once during importance sampling. If an element has been assigned, 
   * its assigned value will be held in its value field. A state consists of the set of variables that have 
   * been assigned, together with the accumulated weight so far. */
  /**
   * Convenience class to store the set of sampled elements, along with the current sampling weight.
   */
  case class State(assigned: Set[Element[_]] = Set(), var weight: Double = 0.0)

  object Reject extends RuntimeException

  /**
   * Create an anytime importance sampler with the given target query elements over the given universe.
   */
  def apply(targets: Element[_]*)(implicit universe: Universe) =
    new Importance(universe, targets: _*) with AnytimeProbQuerySampler

  /**
   * Create an one-time importance sampler with the given target query elements over the given universe
   * using the given number of samples.
   */
  def apply(myNumSamples: Int, targets: Element[_]*)(implicit universe: Universe) =
    new Importance(universe, targets: _*) with OneTimeProbQuerySampler { val numSamples = myNumSamples }
}
