package com.cra.figaro.algorithm.factored

import scala.collection.mutable.Map
import com.cra.figaro.algorithm.sampling.ElementSampler
import com.cra.figaro.language.Element
import com.cra.figaro.util._
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Atomic
import com.cra.figaro.library.atomic.discrete.OneShifter

/**
 * Class to handle sampling from continuous elements in PBP
 * @param argSamples Maximum number of samples to take from atomic elements
 * @param totalSamples Maximum number of samples on the output of chains
 * @param de An instance to compute the density estimate of point during resampling
 */
class ParticleGenerator(de: DensityEstimator) {

  // Caches the samples for an element
  private val sampleMap = Map[Element[_], List[(Double, _)]]()

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
  def update(elem: Element[_], samples: List[(Double, _)]) = sampleMap.update(elem, samples)

  /**
   * Retrieves the samples for an element using the default number of samples.
   */
  def apply[T](elem: Element[T]): List[(Double, T)] = apply(elem, ParticleGenerator.defaultArgSamples)

  /**
   * Retrieves the samples for an element using the indicated number of samples
   */
  def apply[T](elem: Element[T], numSamples: Int): List[(Double, T)] = {
    sampleMap.get(elem) match {
      case Some(e) => {
        e.asInstanceOf[List[(Double, T)]]
      }
      case None => {
        val sampler = ElementSampler(elem, numSamples)
        sampler.start
        val result = sampler.computeDistribution(elem).toList
        sampleMap += elem -> result
        elem.universe.register(sampleMap)
        sampler.kill
        result
      }
    }
  }

  /**
   * Resample and update the element from the indicated beliefs
   * beliefs = (Probability, Value)
   */
  def resample(elem: Element[_], beliefs: List[(Double, _)], proposalVariance: Double): Unit = {

    def nextInt(i: Int) = if (random.nextBoolean) i + 1 else i - 1
    def nextDouble(d: Double) = random.nextGaussian() * proposalVariance + d
    
    val sampleDensity: Double = 1.0 / beliefs.size

    val newSamples = elem match {
      case o: OneShifter => {
        beliefs.map(b => {
          val oldValue = b._2.asInstanceOf[Int]
          val newValue = nextInt(oldValue)
          val nextValue = accept(oldValue, newValue, beliefs.asInstanceOf[List[(Double, Int)]])
          (sampleDensity, nextValue)
        })
      }      
      case a: Atomic[_] => {
        beliefs.map(b => {
          val oldValue = b._2.asInstanceOf[Double]
          val newValue = nextDouble(oldValue)
          val nextValue = accept(oldValue, newValue, beliefs.asInstanceOf[List[(Double, Double)]])
          (sampleDensity, nextValue)
        })        
      }
      case _ => { // Not an atomic element, we don't know how to resample
        beliefs
      }
    }
    update(elem, newSamples)
  }

  def accept[T](oldValue: T, newValue: T, beliefs: List[(Double, T)]): T = {
    val oldDensity = de.getDensity(oldValue, beliefs)
    val newDensity = de.getDensity(newValue, beliefs)
    val ratio = newDensity / oldDensity

    val nextValue = if (ratio > 1) {
      newValue
    } else {
      if (random.nextDouble < ratio) newValue else oldValue
    }
    nextValue
  }
}

object ParticleGenerator {
  var defaultArgSamples = 10
  var defaultTotalSamples = 50

  private val samplerMap: Map[Universe, ParticleGenerator] = Map()

  def clear(univ: Universe) = samplerMap -= univ

  def clear() = samplerMap.clear

  def apply(univ: Universe, de: DensityEstimator): ParticleGenerator =
    samplerMap.get(univ) match {
      case Some(e) => e
      case None => {
        samplerMap += (univ -> new ParticleGenerator(de))
        univ.registerUniverse(samplerMap)
        samplerMap(univ)
      }
    }

  def apply(univ: Universe): ParticleGenerator = apply(univ, new ConstantDensityEstimator)

  def exists(univ: Universe): Boolean = samplerMap.contains(univ)

}