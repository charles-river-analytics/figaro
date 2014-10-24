package com.cra.figaro.experimental.particlebp

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.FactoredAlgorithm
import com.cra.figaro.algorithm.factored.DivideableSemiRing
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.algorithm.factored.Factory
import com.cra.figaro.algorithm.factored.Variable
import com.cra.figaro.algorithm.OneTime
import com.cra.figaro.algorithm.Anytime
import com.cra.figaro.algorithm.factored.LogSumProductSemiring
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.OneTimeProbQuery
import com.cra.figaro.algorithm.factored.Factor
import scala.collection.immutable.Set
import scala.collection.mutable.Map
import com.cra.figaro.algorithm.factored.beliefpropagation.InnerBPHandler
import com.cra.figaro.algorithm.factored.beliefpropagation.OneTimeInnerBPHandler
import com.cra.figaro.algorithm.factored.beliefpropagation.VariableNode
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.factored.DensityEstimator
import com.cra.figaro.algorithm.AnytimeProbQuery
import com.cra.figaro.algorithm.factored.beliefpropagation.AnytimeInnerBPHandler
import com.cra.figaro.algorithm.factored.beliefpropagation.FactorNode
import com.cra.figaro.algorithm.factored.beliefpropagation.Node

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
   * A particle generator to generate particles and do resampling
   */
  val pbpSampler: ParticleGenerator

  /**
   * Variable that if set to true, will preserve parts of the factor graph that cannot
   * change during resampling. This will preserve the messages in those parts of the factor graph.
   * This feature is experimental and not guaranteed to work currently. Default is false.
   */
  val preserveUnchangedGraph: Boolean = false

  /**
   * Elements towards which queries are directed. By default, these are the target elements.
   */
  def starterElements: List[Element[_]] = targetElements

  /*
   * Updates the posterior factors for the specified elements after each inner loop of BP
   * For each of the prior factors, it finds the belief on the corresponding factor in the BP factor
   * graph, and updates the cached version of the factors in the Factory
   */
  /*
  private[figaro] def updatePosteriorFactors(elems: Set[Element[_]]) = {
    for { elem <- elems } {
      val priorFactors = Factory.make(elem).toSet
      val posteriorFactors = bp.factorGraph.getNeighbors(VariableNode(Variable(elem))).map(n => bp.unmakeLogarithmic(bp.belief(n))).toSet
      val newFactors = priorFactors.map(f => {
        val existInNew = posteriorFactors.find(p => p.variables.toSet == f.variables.toSet)
        if (existInNew.nonEmpty) existInNew.get else f
      })
      Factory.updateFactor(elem, newFactors.toList)
    }
  }
  * 
  */

  /*
   * Saves the posterior messages from a factor graph into a map 
   */
  private[figaro] def savePosteriorMessages(elems: Set[Element[_]]): Map[Node, Map[Node, Factor[Double]]] = {
    val oldMsgs = Map[Node, Map[Node, Factor[Double]]]()

    elems.foreach { elem =>
      val priorFactors = Factory.make(elem).toSet

      priorFactors.foreach { pf =>
        val fn: Node = FactorNode(pf.variables.toSet)
        bp.factorGraph.getNeighbors(fn).map(n => {
          val msg = bp.factorGraph.getLastMessage(n, fn)
          oldMsgs.getOrElseUpdate(n, Map[Node, Factor[Double]]()) += fn -> msg
        })
      }
      val vn: Node = VariableNode(Variable(elem))
      bp.factorGraph.getNeighbors(vn).map(n => {
        val msg = bp.factorGraph.getLastMessage(n, vn)
        oldMsgs.getOrElseUpdate(n, Map[Node, Factor[Double]]()) += vn -> msg
      })
    }
    oldMsgs
  }

  /*
   * Updates the messages in the bp factor graph with oldMsgs saved in a map
   */
  private[figaro] def updatePosteriorMessages(oldMsgs: Map[Node, Map[Node, Factor[Double]]]) = {
    oldMsgs.foreach { node =>
      node._2.foreach(to =>
        if (bp.factorGraph.contains(node._1) && bp.factorGraph.contains(to._1)) bp.factorGraph.update(node._1, to._1, to._2))
    }
  }

  /*
   * Runs the inner loop of PBP. 
   * 
   */
  private[figaro] def runInnerLoop(elemsWithPosteriors: Set[Element[_]], dependentElems: Set[Element[_]]) = {
    currentUniverse = universe

    // Save old messages if we are preserving the factor graph
    val oldMsgs: Map[Node, Map[Node, Factor[Double]]] = if (preserveUnchangedGraph) savePosteriorMessages(elemsWithPosteriors) else Map()

    // Remove factors on all elements that can possibly change during resamples
    dependentElems.foreach(Factory.removeFactors(_))

    // Clear the variable and values caches
    Variable.clearCache
    LazyValues.clear(universe)

    // Create BP.
    createBP(targetElements)

    // If we are updating old messages, do it now
    if (oldMsgs.nonEmpty) {
      bp.generateGraph()
      updatePosteriorMessages(oldMsgs)
    }

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
      val oldBeliefs = bp.getBeliefsForElement(elem)
      val bw = proposalEstimator(oldBeliefs)
      val newSamples = pbpSampler.resample(elem, oldBeliefs, bw)
      universe.usedBy(elem)
    }
    (needsToBeResampled, dependentElems)
  }

  /*
   * Runs the outer loop of PBP. 
   */
  private[figaro] def runOuterLoop() = {

    val (needsToBeResampled, dependentElems): (Set[Element[_]], Set[Element[_]]) = if (bp != null) resample() else (Set(), Set())
    val elemsWithPosteriors: Set[Element[_]] = if (bp != null) bp.neededElements.toSet -- dependentElems -- needsToBeResampled else Set()

    runInnerLoop(elemsWithPosteriors, dependentElems)
    //println("Inner loop complete")
  }

  /*
   * Estimates the proposal distribution using the variance of the samples
   */
  def proposalEstimator(beliefs: List[(Double, _)]): Double = {
    val percentOfStd = .1

    beliefs.head._2 match {
      case i: Int => 1.0
      case d: Double => {
        val bd = beliefs.asInstanceOf[List[(Double, Double)]]
        val mean = (0.0 /: bd)((c: Double, n: (Double, Double)) => c + n._1 * n._2)
        val std = math.sqrt((0.0 /: bd)((c: Double, n: (Double, Double)) => c + (n._1 - mean) * (n._1 - mean) * n._2))
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

trait OneTimeParticleBeliefPropagation extends ParticleBeliefPropagation with OneTime with OneTimeInnerBPHandler {
  val outerIterations: Int

  def run() = {
    for { i <- 1 to outerIterations } { runStep() }
  }
}

/**
 * Trait for Anytime BP algorithms
 */
trait AnytimeParticleBeliefPropagation extends ParticleBeliefPropagation with Anytime with AnytimeInnerBPHandler

/**
 * Class to implement a probability query BP algorithm
 */
abstract class ProbQueryParticleBeliefPropagation(numArgSamples: Int, numTotalSamples: Int,
  override val universe: Universe, targets: Element[_]*)(
    //val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
    //val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double,
    depth: Int = Int.MaxValue, upperBounds: Boolean = false)
  extends ProbQueryAlgorithm
  with ParticleBeliefPropagation { //with ProbEvidenceBeliefPropagation {

  val targetElements = targets.toList

  val queryTargets = targetElements

  val semiring = LogSumProductSemiring

  // Dependent stuff not currently implemented
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])] = List()

  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double = {
    (u: Universe, e: List[NamedEvidence[_]]) => () => 1.0
  }

  val densityEstimator = new AutomaticDensityEstimator

  val pbpSampler = ParticleGenerator(universe, densityEstimator, numArgSamples, numTotalSamples)

  /**
   * Getting factors for PBP returns an empty list, since all of the factor creation is handled inside of
   * the BP instances
   */
  def getFactors(neededElements: List[Element[_]],
    targetElements: List[Element[_]], upperBounds: Boolean = false): List[Factor[Double]] = List()

  def createBP(targets: List[Element[_]]): Unit = createBP(targets, depth, upperBounds)

  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = bp.getBeliefsForElement(target).toStream

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    computeDistribution(target).map((pair: (Double, T)) => pair._1 * function(pair._2)).sum
  }

  def computeEvidence(): Double = bp.computeEvidence
}

object ParticleBeliefPropagation {

  /**
   * Creates a One Time belief propagation computer in the current default universe.
   */
  def apply(myOuterIterations: Int, myInnerIterations: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples,
      universe, targets: _*)() with OneTimeParticleBeliefPropagation with OneTimeProbQuery {
      val outerIterations = myOuterIterations
      val innerIterations = myInnerIterations
    }

  def apply(myOuterIterations: Int, myInnerIterations: Int, argSamples: Int, totalSamples: Int,
    targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(argSamples, totalSamples, universe, targets: _*)() with OneTimeParticleBeliefPropagation with OneTimeProbQuery {
      val outerIterations = myOuterIterations
      val innerIterations = myInnerIterations
    }

  def apply(stepTimeMillis: Long, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(ParticleGenerator.defaultArgSamples, ParticleGenerator.defaultTotalSamples,
      universe, targets: _*)() with AnytimeParticleBeliefPropagation with AnytimeProbQuery {
      val myStepTimeMillis = stepTimeMillis
    }

  def apply(stepTimeMillis: Long, argSamples: Int, totalSamples: Int, targets: Element[_]*)(implicit universe: Universe) =
    new ProbQueryParticleBeliefPropagation(argSamples, totalSamples, universe, targets: _*)() with AnytimeParticleBeliefPropagation with AnytimeProbQuery {
      val myStepTimeMillis = stepTimeMillis
    }
}


