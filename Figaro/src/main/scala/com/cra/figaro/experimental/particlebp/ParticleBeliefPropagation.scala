/*
 * ParticleBeliefPropagation.scala
 * A particle belief propagation algorithm
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 20, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.particlebp

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors.{ DivideableSemiRing, Factor, LogSumProductSemiring, Variable }
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm._
import scala.collection.immutable.Set
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.beliefpropagation._
import com.cra.figaro.algorithm.factored._
import breeze.linalg.normalize
import com.cra.figaro.algorithm.UnsupportedAlgorithmException
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.algorithm.factored.factors.factory.Factory

/**
 * Trait for performing particle belief propagation.
 *
 * Only supports Double factors at the moment (i.e., no support for utilities or sufficient statistics)
 */
trait ParticleBeliefPropagation extends FactoredAlgorithm[Double] with InnerBPHandler {

  /**
   * By default, implementations that inherit this trait have no debug information.
   * Override this if you want a debugging option.
   */
  val debug: Boolean = false

  /**
   * The universe on which this belief propagation algorithm should be applied.
   */
  val universe: Universe

  /**
   * Target elements that should not be eliminated but should be available for querying.
   */
  val targetElements: List[Element[_]]

  /**
   * Since BP uses division to compute messages, the semiring has to have a division function defined
   */
  override val semiring: DivideableSemiRing[Double]

  /**
   * The density estimator that will estimate the density of a particle. used for resampling.
   */
  val densityEstimator: DensityEstimator

  /**
   * A particle generator to generate particles and do resampling.
   */
  val pbpSampler: ParticleGenerator

  /**
   * Elements towards which queries are directed. By default, these are the target elements.
   */
  def starterElements: List[Element[_]] = targetElements
  
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
  
  /*
   * Runs the inner loop of PBP. 
   * 
   */
  private[figaro] def runInnerLoop(elemsWithPosteriors: Set[Element[_]], dependentElems: Set[Element[_]]) = {
    currentUniverse = universe

    // Remove factors on all elements that can possibly change during resampluing
    //dependentElems.foreach(Factory.removeFactors(_))    

    // Clear the variable and values caches
    Variable.clearCache()
    LazyValues.clear(universe)

    // Create BP.
    createBP(targetElements, dependentUniverses, dependentAlgorithm)

    // run BP
    runBP()
  }

  /*
   * The resample function. All sampled elements are resampled. For each element that is resampled,
   * we record the dependent elements on those elemens since that portion the factor graph will
   * have to be removed (since resampling can change the structure).
   */
  private[figaro] def resample(): (Set[Element[_]], Set[Element[_]]) = {
    val needsToBeResampled = pbpSampler.sampledElements.filter(e => bp.factorGraph.contains(VariableNode(Variable(e))))
    val dependentElems = needsToBeResampled.flatMap { elem =>
      elem match {
        case a: Atomic[_] =>
        case _ => throw new UnsupportedAlgorithmException(elem)
      }

      // get the beliefs for the element as computed by BP
      val oldBeliefs = bp.getBeliefsForElement(elem)

      // get the last messages to the node that will be used to compute the density  of new samples
      val factors = getLastMessagesToNode(elem)
      val factorBeliefs = factors.map(bp.factorToBeliefs(_))

      // estimate the bandwidth of the proposal using the old belieds 
      val bw = proposalEstimator(oldBeliefs)
      // generate new samples
      val newSamples = pbpSampler.resample(elem, oldBeliefs, factorBeliefs, bw)
      // return the set of dependent elements (and the element itself) that factors will need ot be wipted
      universe.usedBy(elem) + elem
    }
    (needsToBeResampled, dependentElems)
  }

  /* For purposes of resampling, we want to find the belief of the element WITHOUT
    * the original factor. That is, we will incorporate that information using the exact
    * density of the element, we don't need to estimate it from a factor. 
    * 
    * So this function will return all of the last messages to the element node and divide out
    * the original factor 
    * 
    */
  private[figaro] def getLastMessagesToNode(elem: Element[_]): List[Factor[Double]] = {

    // find the node in the graph corresponding to the element
    val elemNode = bp.findNodeForElement(elem)
    val neighbors = bp.factorGraph.getNeighbors(elemNode).toList
    // get the last messages sent to the node
    val lastMessages = neighbors.map(n => (n, bp.factorGraph.getLastMessage(n, elemNode)))
    // find the single variable factor for this node (
    val singleFactorIndex = lastMessages.indexWhere(e => e._1.asInstanceOf[FactorNode].variables.size == 1)
    val singleFactor = if (singleFactorIndex >= 0) lastMessages(singleFactorIndex)
    else throw new UnsupportedAlgorithmException(elem)
    // Get the original factor for this element
    val originalFactor = Factory.makeFactorsForElement(elem)
    if (originalFactor.size > 1) throw new UnsupportedAlgorithmException(elem)
    // Take the single factor, and divide out the original factor. We do this since the single factor in the graph
    // can have evidence multiplied in, so we only want to remove the original factor for it. We will use the original
    // density instead of the factor to estimate densities during resampling
    val factors = lastMessages.patch(singleFactorIndex, Nil, 1).map(_._2) :+ singleFactor._2.combination(bp.makeLogarithmic(originalFactor(0)), bp.semiring.divide)
    factors
  }

  /*
   * Runs the outer loop of PBP. 
   */
  private[figaro] def runOuterLoop() = {

    val (needsToBeResampled, dependentElems): (Set[Element[_]], Set[Element[_]]) = if (bp != null) resample() else (Set(), Set())
    val elemsWithPosteriors: Set[Element[_]] = if (bp != null) bp.neededElements.toSet -- dependentElems -- needsToBeResampled else Set()

    runInnerLoop(elemsWithPosteriors, dependentElems)
  }

  /*
   * Estimates the proposal distribution using the variance of the samples
   */
  private def proposalEstimator(beliefs: List[(Double, _)]): Double = {
    val percentOfStd = .1

    beliefs.head._2 match {
      case i: Int => 1.0
      case d: Double => {
        val bd = beliefs.asInstanceOf[List[(Double, Double)]]
        val mean = (0.0 /: bd)((c: Double, n: (Double, Double)) => c + n._1 * n._2)
        val std = math.sqrt((0.0 /: bd)((c: Double, n: (Double, Double)) => c + (n._2 - mean) * (n._2 - mean) * n._1))
        std * .1
      }
    }

  }

  /**
   * Runs this particle belief propagation algorithm for one iteration. An iteration here is
   * one iteration of the outer loop. This means that the inner BP loop may run several iterations.
   */
  def runStep() {
    runOuterLoop()
  }

}

/**
 * Trait for One time PBP algorithms
 */
trait OneTimeParticleBeliefPropagation extends ParticleBeliefPropagation with OneTime with OneTimeInnerBPHandler {
  val outerIterations: Int

  def run() = {
    for { i <- 1 to outerIterations } { runStep() }
  }
}

/**
 * Trait for Anytime PBP algorithms
 */
trait AnytimeParticleBeliefPropagation extends ParticleBeliefPropagation with Anytime with AnytimeInnerBPHandler {
  override def cleanUp() = if (bp != null) bp.kill
}

/**
 * Class to implement a probability query BP algorithm
 */
abstract class ProbQueryParticleBeliefPropagation(numArgSamples: Int, numTotalSamples: Int,
  override val universe: Universe, targets: Element[_]*)(
    val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    depth: Int = Int.MaxValue, upperBounds: Boolean = false)
  extends ProbQueryAlgorithm
  with ParticleBeliefPropagation { //with ProbEvidenceBeliefPropagation {

  val targetElements = targets.toList

  val queryTargets = targetElements

  val semiring = LogSumProductSemiring()
  
  val densityEstimator = new AutomaticDensityEstimator

  val pbpSampler = ParticleGenerator(universe, densityEstimator, numArgSamples, numTotalSamples)

  /**
   * Getting factors for PBP returns an empty list, since all of the factor creation is handled inside of
   * the BP instances
   */
  def getFactors(neededElements: List[Element[_]],
    targetElements: List[Element[_]], upperBounds: Boolean = false): List[Factor[Double]] = List()  

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = bp.getBeliefsForElement(target).toStream

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }

}

object ParticleBeliefPropagation {

  /**
   * Creates a One Time belief propagation computer in the current default universe.
   */
  def apply(myOuterIterations: Int, myInnerIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples,
      universe, targets: _*)(List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeParticleBeliefPropagation with OneTimeProbQuery {
      val outerIterations = myOuterIterations
      val innerIterations = myInnerIterations
    }

  /**
   * Creates a One Time belief propagation computer in the current default universe. Use the dependent universe and algorithm to compute prob of evidence in dependent universe
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, myOuterIterations: Int, myInnerIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples,
      universe, targets: _*)(dependentUniverses, dependentAlgorithm) with OneTimeParticleBeliefPropagation with OneTimeProbQuery {
      val outerIterations = myOuterIterations
      val innerIterations = myInnerIterations
    }

  /**
   * Creates a One Time belief propagation computer in the current default universe that specifies the number of samples to take for each element.
   */
  def apply(myOuterIterations: Int, myInnerIterations: Int, argSamples: Int, totalSamples: Int,
    targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(argSamples, totalSamples, universe, targets: _*)(List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with OneTimeParticleBeliefPropagation with OneTimeProbQuery {
      val outerIterations = myOuterIterations
      val innerIterations = myInnerIterations
    }

  /**
   * Creates a One Time belief propagation computer in the current default universe that specifies the number of samples to take for each element.
   * Use the dependent universe and algorithm to compute prob of evidence in dependent universe
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, myOuterIterations: Int, myInnerIterations: Int, argSamples: Int, totalSamples: Int,
    targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(argSamples, totalSamples, universe, targets: _*)(dependentUniverses, dependentAlgorithm) with OneTimeParticleBeliefPropagation with OneTimeProbQuery {
      val outerIterations = myOuterIterations
      val innerIterations = myInnerIterations
    }

  /**
   * Creates a Anytime belief propagation computer in the current default universe.
   */
  def apply(stepTimeMillis: Long, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples,
      universe, targets: _*)(List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with AnytimeParticleBeliefPropagation with AnytimeProbQuery {
      val myStepTimeMillis = stepTimeMillis
    }

  /**
   * Creates a Anytime belief propagation computer in the current default universe. Use the dependent universe and algorithm to compute prob of evidence in dependent universe
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, stepTimeMillis: Long, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples,
      universe, targets: _*)(dependentUniverses, dependentAlgorithm) with AnytimeParticleBeliefPropagation with AnytimeProbQuery {
      val myStepTimeMillis = stepTimeMillis
    }

  /**
   * Creates a Anytime belief propagation computer in the current default universe that specifies the number of samples to take for each element.
   */
  def apply(stepTimeMillis: Long, argSamples: Int, totalSamples: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(argSamples, totalSamples, universe, targets: _*)(List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) with AnytimeParticleBeliefPropagation with AnytimeProbQuery {
      val myStepTimeMillis = stepTimeMillis
    }

  /**
   * Creates a Anytime belief propagation computer in the current default universe that specifies the number of samples to take for each element.
   *  Use the dependent universe and algorithm to compute prob of evidence in dependent universe
   */
  def apply(dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double, stepTimeMillis: Long, argSamples: Int, totalSamples: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(argSamples, totalSamples, universe, targets: _*)(dependentUniverses, dependentAlgorithm) with AnytimeParticleBeliefPropagation with AnytimeProbQuery {
      val myStepTimeMillis = stepTimeMillis
    }
}


