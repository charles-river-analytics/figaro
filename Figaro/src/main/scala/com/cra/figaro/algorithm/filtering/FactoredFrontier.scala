/*
 * FactoredFrontier.scala  
 * A factored filtering algorithm.
 * 
 * Created By:      William Kretschmer, Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jul 16, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.algorithm.factored.beliefpropagation._
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.algorithm.{OneTimeProbQuery, AnytimeProbQuery}
import com.cra.figaro.language._

/**
 * Abstract class that runs the Factored Frontier algorithm.
 * 
 * @param static The universe of elements that do not change over time.
 * @param initial The universe describing the distribution over the initial state of the system.
 * @param transition The transition model describing how the current state of the system depends on the previous.
 */
abstract class FactoredFrontier(static: Universe, initial: Universe, transition: (Universe, Universe) => Universe)
  extends Filtering(static, initial, transition(static, _)) with OneTimeFiltering with BPHandler {
  
  protected var currentStatic = static
  protected var currentUniverse = initial
  
  protected var bp: ProbQueryBeliefPropagation = _
  
  override def initialize() {
    LazyValues.clear(static)
    createBP(getNamedElements(currentUniverse) ::: getNamedElements(currentStatic))
  }
  
  def run() {
    runBP()
  }
  
  override def cleanUp() {
    LazyValues.clear(currentUniverse)
    bp.kill()
    if(currentStatic != static) currentStatic.clear() else LazyValues.clear(currentStatic)
  }
  
  /*
   * Creates a dummy universe based on the final factors of the corresponding element from the last iteration of BP.
   * All elements become atomic selects, thus preventing a memory leak as the algorithm runs over time.
   */
  private def createDummyUniverse(u: Universe): Universe = {
    val dummyUniverse = new Universe
    for(e <- getNamedElements(u)) {
      Select(bp.getBeliefsForElement(e):_*)(e.name.string, dummyUniverse)
    }
    dummyUniverse
  }
  
  /**
   * Advance the algorithm one time step based on the provided evidence.
   */
  def advanceTime(evidence: Seq[NamedEvidence[_]] = List()): Unit = {    
    val previousUniverse = currentUniverse
    val previousStatic = currentStatic
    val dummyUniverse = createDummyUniverse(previousUniverse)
    currentStatic = createDummyUniverse(previousStatic)
    
    LazyValues.clear(previousUniverse)
    bp.kill()
    if(previousStatic != static) previousStatic.clear() else LazyValues.clear(previousStatic)
    
    currentUniverse = transition(currentStatic, dummyUniverse)
    currentUniverse.assertEvidence(evidence)
    
    createBP(getNamedElements(currentUniverse) ::: getNamedElements(currentStatic) ::: getNamedElements(dummyUniverse))
    runBP()
    
    dummyUniverse.clear()
  }
  
  /**
   * Returns the distribution over the element referred to by the reference at the current time point.
   */
  def computeCurrentDistribution[T](reference: Reference[T]): Stream[(Double, T)] = {
    try{
      bp.computeDistribution(currentUniverse.getElementByReference(reference))
    } catch {
      case _: NoSuchElementException => bp.computeDistribution(currentStatic.getElementByReference(reference))
    }
  }
  
  /**
   * Returns the expectation of the element referred to by the reference
   * under the given function at the current time point.
   */
  def computeCurrentExpectation[T](reference: Reference[T], function: T => Double): Double = {
    try{
      bp.computeExpectation(currentUniverse.getElementByReference(reference), function)
    } catch {
      case _: NoSuchElementException => bp.computeExpectation(currentStatic.getElementByReference(reference), function)
    }
  }
}

/**
 * Trait for creating and running Belief Propagation within Factored Frontier. BP runs as either a one time or anytime algorithm.
 */
trait BPHandler {
  /**
   * Dummy static universe associated with this time step.
   */
  protected var currentStatic: Universe
  
  /**
   * Current universe associated with this time step.
   */
  protected var currentUniverse: Universe
  
  /**
   * BP algorithm associated with this time step.
   */
  protected var bp: ProbQueryBeliefPropagation
  
  /**
   * Instantiates the appropriate BP algorithm for the current time step.
   */
  protected def createBP(targets: List[Element[_]]): Unit
  
  /**
   * Runs the BP algorithm at the current time step.
   */
  protected def runBP(): Unit
  
  /**
   * Returns all named elements in this universe.
   */
  protected def getNamedElements(u: Universe): List[Element[_]] = {
    u.activeElements.filterNot(_.name.isEmpty)
  }
}

/**
 * Trait for running Factored Frontier with one time Belief Propagation.
 */
trait OneTimeBPHandler extends BPHandler {
  /**
   * Number of iterations to run BP per step.
   */
  val myIterations: Int
  
  protected def createBP(targets: List[Element[_]]) {
    bp = new ProbQueryBeliefPropagation(currentUniverse, targets:_*)(List(), (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with OneTimeProbabilisticBeliefPropagation with OneTimeProbQuery { override val iterations = myIterations }
  }
  
  protected def runBP() {
    bp.start()
  }
}

/**
 * Trait for running Factored Frontier with anytime Belief Propagation.
 */
trait AnytimeBPHandler extends BPHandler {
  /**
   * Time, in milliseconds, to run BP per step.
   */
  val myStepTimeMillis: Long
  
  protected def createBP(targets: List[Element[_]]) {
    bp = new ProbQueryBeliefPropagation(currentUniverse, targets:_*)(List(), (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
      with AnytimeProbabilisticBeliefPropagation with AnytimeProbQuery
  }
  
  protected def runBP() {
    bp.start()
    Thread.sleep(myStepTimeMillis)
    bp.stop()
  }
}

object FactoredFrontier {
  /**
   * A Factored Frontier that runs with one time Belief Propagation and a static universe.
   * 
   * @param static The universe of elements that do not change over time.
   * @param initial The universe describing the distribution over the initial state of the system.
   * @param transition The transition model describing how the current state of the system depends on the previous.
   * @param iterations The number of iterations with which to run Belief Propagation at each time step.
   */
  def apply(static: Universe, initial: Universe, transition: (Universe, Universe) => Universe, iterations: Int): FactoredFrontier = 
    new FactoredFrontier(static, initial, transition) with OneTimeBPHandler { val myIterations = iterations }
    
  /**
   * A Factored Frontier that runs with one time Belief Propagation.
   * 
   * @param initial The universe describing the distribution over the initial state of the system.
   * @param transition The transition model describing how the current state of the system depends on the previous.
   * @param iterations The number of iterations with which to run Belief Propagation at each time step.
   */
  def apply(initial: Universe, transition: Universe => Universe, iterations: Int): FactoredFrontier = 
    apply(new Universe(), initial, (static: Universe, previous: Universe) => transition(previous), iterations)
    
  /**
   * A Factored Frontier that runs with anytime Belief Propagation and a static universe.
   * 
   * @param static The universe of elements that do not change over time.
   * @param initial The universe describing the distribution over the initial state of the system.
   * @param transition The transition model describing how the current state of the system depends on the previous.
   * @param stepTimeMillis The time, in milliseconds, for which to run Belief Propagation at each time step.
   */
  def apply(static: Universe, initial: Universe, transition: (Universe, Universe) => Universe, stepTimeMillis: Long): FactoredFrontier = 
    new FactoredFrontier(static, initial, transition) with AnytimeBPHandler { val myStepTimeMillis = stepTimeMillis }
    
  /**
   * A Factored Frontier that runs with anytime Belief Propagation.
   * 
   * @param initial The universe describing the distribution over the initial state of the system.
   * @param transition The transition model describing how the current state of the system depends on the previous.
   * @param stepTimeMillis The time, in milliseconds, for which to run Belief Propagation at each time step.
   */
  def apply(initial: Universe, transition: Universe => Universe, stepTimeMillis: Long): FactoredFrontier =
    apply(new Universe(), initial, (static: Universe, previous: Universe) => transition(previous), stepTimeMillis)
}