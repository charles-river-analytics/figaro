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
 *  Likelihood weighting works by propagating observations through Dists and Chains
 *  to the variables they depend on. If we don't make sure we sample those Dists and
 *  Chains first, we may end up sampling those other elements without the correct
 *  observations. To avoid this, we keep track of all these dependencies.
 *  The dependencies map contains all the elements that could propagate an
 *  observation to any given element.
 *  Note: the dependencies map is only concerned with active elements that are present 
 *  at the beginning of sampling (even though we get a new active elements list each sample).
 *  Temporary elements will always be created after the element that could propagate
 *  an observation to them, because that propagation has to go through a permanent 
 *  element.
 *  Therefore, we can generate the dependencies map once before all the samples are generated.
 */  
  private val dependencies = scala.collection.mutable.Map[Element[_], Set[Element[_]]]()
  private def makeDependencies() = {
    for {
      element <- universe.activeElements
    } {
      element match {
        case d: Dist[_,_] => 
          for { o <- d.outcomes } { dependencies += o -> (dependencies.getOrElse(o, Set()) + d) }
        case c: CachingChain[_,_] => 
          val outcomes = Values(universe)(c.parent).map(c.get(_))
          for { o <- outcomes } { dependencies += o -> (dependencies.getOrElse(o, Set()) + c) }
        case _ => ()
      }
    }
  }
  makeDependencies()
  
  private var numRejections = 0
  private var logSuccessWeight = 0.0
  private var numSamples = 0
  
  override protected def resetCounts() {
    super.resetCounts()
    numRejections = 0
    logSuccessWeight = 0.0
    numSamples = 0
  }
  
  
  /*
   * Produce one weighted sample of the given element. weightedSample takes into account conditions and constraints
   * on all elements in the Universe, including those that depend on this element.
   */
  @tailrec final def sample(): Sample = {
    /* 
     * We need to recreate the activeElements each sample, because non-temporary elements may have been made active
     * in a previous iteration. See the relevant test in ImportanceTest.
     */    
    val activeElements = universe.activeElements
    val resultOpt: Option[Sample] =
      try {
        val state = State()
        activeElements.foreach(e => if (e.active) sampleOne(state, e, None))
        val bindings = targets map (elem => elem -> elem.value)
        Some((state.weight, Map(bindings: _*)))
      } catch {
        case Importance.Reject =>
          None
      }

    resultOpt match {
      case Some(x) =>
        logSuccessWeight = logSum(logSuccessWeight, x._1)
        numSamples += 1
        x
      case None =>
        numRejections += 1
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
    /*
     * We have to make sure to sample any elements this element depends on first so we can get the right
     * observation for this element.
     */
    dependencies.getOrElse(element, Set()).filter(!state.assigned.contains(_)).foreach(sampleOne(state, _, None))
    if (element.universe != universe || (state.assigned contains element)) {
      element.value
    }
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
      if (fullObservation.isEmpty || !element.isInstanceOf[HasDensity[_]]) {
        val result = sampleValue(state, element, fullObservation)
        if (!element.condition(result)) throw Importance.Reject
        result
      } else {
        // Optimize the common case of an observation on an atomic element.
        // This partially implements likelihood weighting by clamping the element to its
        // desired value and multiplying the weight by the density of the value.
        // This can dramatically reduce the number of rejections.
        element.args.foreach(sampleOne(state, _, None))
        val obs = fullObservation.get

        // Subtle issue taken care of by the following line
        // A parameterized element may or may not be a chain to an atomic element
        // If it's not, we have to make sure to set its value to the observation here
        // If it is, we have to make sure to propagate the observation through the chain
        sampleValue(state, element, Some(obs))       
        state.weight += math.log(element.asInstanceOf[HasDensity[T]].density(obs))
        obs
      }
    element.value = value
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
        val next = c.get(parentValue)
        c.value = sampleOne(state, next, observation)
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
      case _ =>
        (element.args ::: element.elementsIAmContingentOn.toList) foreach (sampleOne(state, _, None))
        element.randomness = element.generateRandomness()
        element.value = element.generateValue(element.randomness)
        element.value
    }
  }

  def logProbEvidence: Double = {
	logSuccessWeight - Math.log(numSamples + numRejections)
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
    new Importance(universe, targets: _*) with OneTimeProbQuerySampler { 
      val numSamples = myNumSamples 

    /**
      * Use one-time sampling to compute the probability of the given named evidence.
      * Takes the conditions and constraints in the model as part of the model definition.
      * This method takes care of creating and running the necessary algorithms.
      */
    def probabilityOfEvidence(evidence: List[NamedEvidence[_]]): Double = {
      val logPartition = logProbEvidence
      universe.assertEvidence(evidence)
      if (active) kill()
      start()
      Math.exp(logProbEvidence - logPartition)
    }
      
  }
  
  /**
   * Use IS to compute the probability that the given element has the given value.
   */    
  def probability[T](target: Element[T], value: T, numSamples: Int = 10000): Double = {
    val alg = Importance(numSamples, target)
    alg.start()
    val result = alg.probability(target, value)
    alg.kill()
    result
  }    
  
}
