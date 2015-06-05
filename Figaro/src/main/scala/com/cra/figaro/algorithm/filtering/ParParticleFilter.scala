package com.cra.figaro.algorithm.filtering

import com.cra.figaro.language._
import scala.collection.parallel.ParSeq

/**
 * A parallel implementation of a OneTimeParticleFilter.
 */
// TODO: could static just be a universe?
class ParOneTimeParticleFilter(static: () => Universe, initial: () => Universe, transition: (Universe, Universe) => Universe, val numParticles: Int, numThreads: Int)
  extends ParFiltering(transition) with ParticleFilter {
  
  // sequence of UniverseWindows -- one for each thread
  var windows: Seq[UniverseWindow] = _
  
  // (start, end) indices for each thread to divide up numParticles
  val indices = calculateIndices(numParticles, numThreads)
  
  private def doTimeStep(weightedParticles: Seq[ParticleFilter.WeightedParticle]) {
    // compute probability of evidence here by taking the average weight of the weighted particles and store it so you can later return it as a query result
    computeProbEvidence(weightedParticles)

    updateBeliefState(weightedParticles)
  }
  
  def run(): Unit = {
    windows = (1 to numThreads) map { _ => new UniverseWindow(null, initial(), static()) }
    val particleLists = windows.par zip indices map { case(window, (start, end)) =>
      (start to end) map { _ => initialWeightedParticle(window.static, window.current)}
      }
    val particles = particleLists.flatten.toList
    doTimeStep(particles)
  }
  
  def advanceTime(evidence: Seq[NamedEvidence[_]] = List()): Unit = {
    val newWindows = windows map { window => 
      advanceUniverse(window, transition)
    }
    val particleLists = newWindows.par zip indices map { case (window, (start,end)) =>
      (start to end) map { i => addWeightedParticle(evidence, i, window)
      }
    }
    val particles = particleLists.flatten.toList
    doTimeStep(particles)
    windows = newWindows
  }
  
  /**
   * Calculate start and end indices for each thread dividing up the particles
   */
  private def calculateIndices(numParticles: Int, numThreads: Int): Seq[(Int, Int)] = {
    val indices = (1 to numThreads) map { i =>
      val start = (i - 1) * numParticles / numThreads
      val end = i * numParticles / numThreads -1
      (start, end)
    }
    indices
  }
}

object ParParticleFilter {

  def apply(static: () => Universe, initial: () => Universe, transition: (Universe, Universe) => Universe, numParticles: Int, numThreads: Int): ParOneTimeParticleFilter =
    new ParOneTimeParticleFilter(static, initial, transition, numParticles, numThreads)

  def apply(initial: () => Universe, transition: Universe => Universe, numParticles: Int, numThreads: Int): ParOneTimeParticleFilter =
    apply(() => new Universe(), initial, (static: Universe, previous: Universe) => transition(previous), numParticles, numThreads)

}