package com.cra.figaro.algorithm.filtering

import com.cra.figaro.language._
import scala.collection.parallel.ParSeq

/**
 * A parallel implementation of a OneTimeParticleFilter.
 */
// TODO: could static just be a universe?
class ParOneTimeParticleFilter(static: () => Universe, initial: () => Universe, transition: (Universe, Universe) => Universe, val numParticles: Int, numThreads: Int)
  extends ParFiltering(transition) with ParticleFilter {
  
  var windows: Seq[UniverseWindow] = _
  
  private def doTimeStep(weightedParticles: Seq[ParticleFilter.WeightedParticle]) {
    // compute probability of evidence here by taking the average weight of the weighted particles and store it so you can later return it as a query result
    computeProbEvidence(weightedParticles)

    updateBeliefState(weightedParticles)
  }
  
  def run(): Unit = {
    windows = (1 to numThreads) map { _ => new UniverseWindow(null, initial(), static()) }
    val particleLists = windows.par.zipWithIndex map { case (window, i) =>
      val start = (i - 1) * numParticles / numThreads
      val end = i * numParticles / numThreads
      (start until end) map { _ => initialWeightedParticle(window.static, window.current)}
    }
    val particles = particleLists.flatten.toList
    doTimeStep(particles)
  }
  
  def advanceTime(evidence: Seq[NamedEvidence[_]] = List()): Unit = {
    val newWindows = windows map { window => 
      advanceUniverse(window, transition)
    }
    val indexes = (1 to numThreads) map { i =>
      val start = (i - 1) * numParticles / numThreads
      val end = i * numParticles / numThreads
      (start until end).toList
    }
    val particleLists = newWindows.par zip indexes map { case (window, range) =>
      range map { i =>
        addWeightedParticle(evidence, i, window)
      }
    }
    val particles = particleLists.flatten.toList
    doTimeStep(particles)
    windows = newWindows
  }
}

object ParParticleFilter {

  def apply(static: () => Universe, initial: () => Universe, transition: (Universe, Universe) => Universe, numParticles: Int, numThreads: Int): ParOneTimeParticleFilter =
    new ParOneTimeParticleFilter(static, initial, transition, numParticles, numThreads)

  def apply(initial: () => Universe, transition: Universe => Universe, numParticles: Int, numThreads: Int): ParOneTimeParticleFilter =
    apply(() => new Universe(), initial, (static: Universe, previous: Universe) => transition(previous), numParticles, numThreads)

}