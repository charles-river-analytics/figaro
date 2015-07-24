/*
 * ParParticleFilter.scala
 * A parallel one-time particle filter.
 * 
 * Created By:      Lee Kellogg (lkellogg@cra.com)
 * Creation Date:   Jun 2, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.filtering

import com.cra.figaro.language._
import scala.collection.parallel.ParSeq
import com.cra.figaro.algorithm.filtering.ParticleFilter.WeightedParticle
import com.cra.figaro.library.cache.PermanentCache
import com.cra.figaro.library.cache.Cache
import com.cra.figaro.algorithm.sampling.LikelihoodWeighter

/**
 * A parallel one-time particle filter. Distributes the work of generating particles at each time step over a specified
 * number of threads. After generating the particles, they are recombined before re-sampling occurs. Instead of accepting
 * initial and static universes as input, this method accepts functions that return universes. This is because each 
 * thread needs its own set of universes to work on. It is important that any elements created within those functions are
 * explicitly assigned to the returned universe, not the implicit default universe.
 * 
 * @param static A function that returns a universe of elements whose values do not change over time
 * @param initial A function that returns a universe describing the distribution over the initial state of the system
 * @param transition The transition model describing how the current state of the system depends on the static and previous, respectively
 * @param numParticles Number of particles to use at each time step
 * @param numThreads The number of threads over which to distribute the work of generating the particles at each step
 */
class ParOneTimeParticleFilter(static: () => Universe, initial: () => Universe, transition: (Universe, Universe) => Universe, val numParticles: Int, numThreads: Int)
  extends ParFiltering(transition) with ParticleFilter {
  
  /** sequence of UniverseWindows -- one for each thread */
  private var windows: Seq[UniverseWindow] = _
  
  /** (start, end) indices for each thread to divide up numParticles */
  private val indices = calculateIndices(numParticles, numThreads)
  
  /** generate the initial UniverseWindows */
  private def genInitialWindows(): Seq[UniverseWindow] = {
    Seq.fill(numThreads)(new UniverseWindow(null, initial(), static()))
  }
  
  /** apply the transition function to each of a sequence of UniverseWindows */
  private def advanceUniverseWindows(windows: Seq[UniverseWindow]): Seq[UniverseWindow] = {
    windows map { w => advanceUniverse(w, transition) }
  }
  
  /**
   * generate particles for each thread, in parallel, then recombine and return
   * 
   * @param windows the UniverseWindows to sample from
   * @param weightedParticleCreator a function that generates a WeightedParticle, given a UniverseWindow and an index
   */
  private def genParticles(windows: Seq[(UniverseWindow, LikelihoodWeighter)], weightedParticleCreator: ((UniverseWindow, LikelihoodWeighter), Int) => WeightedParticle): Seq[WeightedParticle] = {
    val parWindows = windows.par
    val particles = parWindows zip indices flatMap { case(window, (start, end)) =>
      (start to end) map { i => weightedParticleCreator(window, i) }
    }
    particles.seq
  }
  
  /** compute probability of evidence for the particles, and update the belief state (after re-sampling) */
  private def doTimeStep(weightedParticles: Seq[WeightedParticle]) {
    computeProbEvidence(weightedParticles)
    updateBeliefState(weightedParticles)
  }
  
  def run(): Unit = {
    windows = genInitialWindows()
    val windowWithCaches = windows.map(w => (w, new LikelihoodWeighter(w.current, new PermanentCache(w.current))))
    val particles = genParticles(windowWithCaches, (w, _) => initialWeightedParticle(w._1.static, w._1.current, w._2))
    doTimeStep(particles)
  }
  
  def advanceTime(evidence: Seq[NamedEvidence[_]] = List()): Unit = {
    val newWindows = advanceUniverseWindows(windows)
    val newWindowsWithCaches = newWindows.map(w => (w, new LikelihoodWeighter(w.current, new PermanentCache(w.current))))
    val particles = genParticles(newWindowsWithCaches, (w, i) => addWeightedParticle(evidence, i, w._1, w._2))
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

/**
 * A parallel implementation of a OneTimeParticleFilter.
 */
object ParParticleFilter {

  /**
   * A parallel one-time particle filter. Distributes the work of generating particles at each time step over a specified
   * number of threads. After generating the particles, they are recombined before re-sampling occurs. Instead of accepting
   * initial and static universes as input, this method accepts functions that return universes. This is because each 
   * thread needs its own set of universes to work on. It is important that any elements created within those functions are
   * explicitly assigned to the returned universe, not the implicit default universe.
   * 
   * 
   * @param static A function that returns a universe of elements whose values do not change over time
   * @param initial A function that returns a universe describing the distribution over the initial state of the system
   * @param transition The transition model describing how the current state of the system depends on the static and previous, respectively
   * @param numParticles Number of particles to use at each time step
   * @param numThreads The number of threads over which to distribute the work of generating the particles at each step
   */
  def apply(static: () => Universe, initial: () => Universe, transition: (Universe, Universe) => Universe, numParticles: Int, numThreads: Int): ParOneTimeParticleFilter =
    new ParOneTimeParticleFilter(static, initial, transition, numParticles, numThreads)

  /**
   * A parallel one-time particle filter. Distributes the work of generating particles at each time step over a specified
   * number of threads. After generating the particles, they are recombined before re-sampling occurs. Instead of accepting
   * an initial universe as input, this method accepts a function that returns a universe. This is because each thread needs
   * its own set of universes to work on. It is important that any elements created within that function are explicitly
   * assigned to the returned universe, not the implicit default universe.
   * 
   * @param initial A function that returns a universe describing the distribution over the initial state of the system
   * @param transition The transition model describing how the current state of the system depends on the previous
   * @param numParticles Number of particles to use at each time step
   * @param numThreads The number of threads over which to distribute the work of generating the particles at each step
   */
  def apply(initial: () => Universe, transition: Universe => Universe, numParticles: Int, numThreads: Int): ParOneTimeParticleFilter =
    apply(() => new Universe(), initial, (static: Universe, previous: Universe) => transition(previous), numParticles, numThreads)

}