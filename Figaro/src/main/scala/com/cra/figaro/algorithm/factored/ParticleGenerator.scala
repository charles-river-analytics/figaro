/*
 * ParticleGenerator.scala
 * Class to handle sampling from continuous elements in PBP
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 8, 2014
 * 
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored

import scala.collection.mutable.Map
import com.cra.figaro.algorithm.sampling.ElementSampler
import com.cra.figaro.language.Element
import com.cra.figaro.util._
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Atomic
import com.cra.figaro.library.atomic.discrete.OneShifter
import com.cra.figaro.util.MapResampler
import com.cra.figaro.algorithm.factored.factors.Factor

/**
 * Class to handle sampling from continuous elements in PBP
 * @param argSamples Maximum number of samples to take from atomic elements
 * @param totalSamples Maximum number of samples on the output of chains
 * @param de An instance to compute the density estimate of point during resampling
 */
class ParticleGenerator(de: DensityEstimator, val numArgSamples: Int, val numTotalSamples: Int) {

  // Caches the samples for an element
  private val sampleMap = Map[Element[_], (List[(Double, _)], Int)]()

  /**
   * Returns the set of sampled elements contained in this sampler
   */
  def sampledElements(): Set[Element[_]] = sampleMap.keySet.toSet

  /**
   * Clears all of the samples for elements in this sampler
   */
  def clear() = sampleMap.clear

  /**
   * Updates the samples for an element
   */
  def update(elem: Element[_], numSamples: Int, samples: List[(Double, _)]) = sampleMap.update(elem, (samples, numSamples))

  /**
   * Retrieves the samples for an element using the default number of samples.
   */
  def apply[T](elem: Element[T]): List[(Double, T)] = apply(elem, numArgSamples)

  /**
   * Retrieves the samples for an element using the indicated number of samples
   */
  def apply[T](elem: Element[T], numSamples: Int): List[(Double, T)] = {
    sampleMap.get(elem) match {
      case Some(e) => {
        e.asInstanceOf[(List[(Double, T)], Int)]._1
      }
      case None => {
        val sampler = ElementSampler(elem, numSamples)
        sampler.start
        val result = sampler.computeDistribution(elem).toList
        sampleMap += elem -> (result, numSamples)
        elem.universe.register(sampleMap)
        sampler.kill
        result
      }
    }
  }

  /**
   * Resample and update the element from the indicated beliefs
   * beliefs/oldMessages = (Probability, Value)
   */
  def resample(elem: Element[_], beliefs: List[(Double, _)], oldMessages: List[List[(Double, _)]], proposalVariance: Double): Unit = {

    /* MH Proposal for Double type. 
     * TODO: Integrate these proposals with existing MH algorithms
     */
    def nextDouble(d: Double) = random.nextGaussian() * proposalVariance + d

    val numSamples = sampleMap(elem)._2
    
    val sampleDensity: Double = 1.0 / numSamples
       
    // Generate new samples given the old samples for an element 
    val newSamples = elem match {
      /* If the element is an instance of OneShifter (Geometric, poisson, etc),
       * we first resample the existing beliefs so there are numSample particles of equal weight.
       */
      case o: OneShifter => {
        val toResample = if (beliefs.size < numSamples) {
          val resampler = new MapResampler(beliefs.map(s => (s._1, s._2)))
          List.fill(numSamples)(1.0/numSamples, resampler.resample)
        } else {
          beliefs
        }
        /*
         * Now for each particle, choose to resample the particle based on a proposal.
         * Use the OneShifter class to generate the proposal distribution.
         * Call the accept function to determine if we accept the proposal or stick with the original
         * Note, if we propose something that has zero density, it is always rejected
         */
        val samples = toResample.map(b => {
          val oldValue = b._2.asInstanceOf[Int]
          val newValue = o.shiftOne(oldValue)
          val nextValue = if (o.density(newValue._1) > 0.0) {
            accept(o, oldValue, newValue._1, newValue._2, oldMessages.asInstanceOf[List[List[(Double, Int)]]])
          } else oldValue
          (sampleDensity, nextValue)
        })
        // return the new particles
        samples.groupBy(_._2).toList.map(s => (s._2.unzip._1.sum, s._2.head._2))
      }
      
      /* For atomic doubles, we do the same thing as the OneShifters, but we assume
       * that we never need to resample since the number of particles equals numSamples.
       * We propose a new double and check its acceptance. Note the proposal is symmetric.
       */
      case a: Atomic[_] => { // The double is unchecked, bad stuff if the atomic is not double
        beliefs.map(b => {
          val oldValue = b._2.asInstanceOf[Double]
          val newValue = nextDouble(oldValue)
          val nextValue = if (a.asInstanceOf[Atomic[Double]].density(newValue) > 0.0) {
            accept(a, oldValue, newValue, 1.0, oldMessages.asInstanceOf[List[List[(Double, Double)]]])
          } else oldValue
          (sampleDensity, nextValue)
        })
      }
      case _ => { // Not an atomic element, we don't know how to resample
        beliefs
      }
    }
    update(elem, numSamples, newSamples)
  }

  /*
   *  Determine if we accept a new sample or not. We compute the acceptance probability as the ratio of the old and new density times
   *  the proposal probability. To compute the densities, we take a list of beliefs over the element, and for each belief,
   *  we estimate the density using the density estimator, then multiple all of the estimates together. Finally, since
   *  we only sample atomic elements, we multiple each result but the density of the values in the original element 
   */
  private def accept[T](elem: Atomic[_], oldValue: T, newValue: T, proposalProb: Double, beliefs: List[List[(Double, T)]]): T = {        
    val oldDensity = beliefs.map(de.getDensity(oldValue, _)).product*elem.asInstanceOf[Atomic[T]].density(oldValue)
    val newDensity = beliefs.map(de.getDensity(newValue, _)).product*elem.asInstanceOf[Atomic[T]].density(newValue)
    val ratio = (newDensity / oldDensity) * proposalProb

    val nextValue = if (ratio > 1) {
      newValue
    } else {
      if (random.nextDouble < ratio) newValue else oldValue
    }
    nextValue
  }
}

object ParticleGenerator {
  /**
   * Maximum number of particles to generate per atomic
   */
  var defaultArgSamples = 15
  
  /**
   * Maximum number of particles to generate through a chain.
   */
  var defaultTotalSamples = 15

  private val samplerMap: Map[Universe, ParticleGenerator] = Map()

  /**
   * Clear the particle generate for a universe
   */
  def clear(univ: Universe) = samplerMap -= univ

  /**
   * Clear all particle generators
   */
  def clear() = samplerMap.clear

  /**
   * Create a new particle generator for the given universe, using the given density estimatore, number of argument samples and total number of samples
   */
  def apply(univ: Universe, de: DensityEstimator, numArgSamples: Int, numTotalSamples: Int): ParticleGenerator =
    samplerMap.get(univ) match {
      case Some(e) => e
      case None => {
        samplerMap += (univ -> new ParticleGenerator(de, numArgSamples, numTotalSamples))
        univ.registerUniverse(samplerMap)
        samplerMap(univ)
      }
    }

  /**
   * Create a new particle generate for a universe using a constant density estimator and default samples
   */
  def apply(univ: Universe): ParticleGenerator = apply(univ, new ConstantDensityEstimator,
      defaultArgSamples, defaultTotalSamples)

  /**
   * Check if a particle generate exists for this universe
   */
  def exists(univ: Universe): Boolean = samplerMap.contains(univ)

}
