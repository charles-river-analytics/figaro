/*
 * WeightedSampler.scala
 * Weighted samplers.
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
import scala.collection.mutable.Map
import scala.language.postfixOps
import com.cra.figaro.util._

/**
 * Samplers that use weighted samples.
 */
abstract class WeightedSampler(override val universe: Universe, targets: Element[_]*) extends ProbQuerySampler with Sampler with StreamableProbQueryAlgorithm {
  lazy val queryTargets = targets.toList
  /**
   * A sample consists of a weight and a map from elements to their values.
   */
  type Sample = (Double, Map[Element[_], Any])

  /**
   * Produce a single sample.
   */
  def sample(): Sample

  protected var totalWeight: Double = _

  /**
   * Total weight of samples taken.
   */
  def getTotalWeight = totalWeight

  // To save memory, we do not store the entire list of samples. Rather, we compile, for each target, how many times
  // each value of the target has been seen. If the same values appear many times for a target, considerable space
  // will be saved. However, if every sample produces a different value for the target, the memory requirements will
  // still be substantial.

  protected type WeightSeen[T] = (Element[T], Map[T, Double])

  protected def newWeightSeen[T](target: Element[T]): WeightSeen[T] = (target, Map())

  protected var allWeightsSeen: List[WeightSeen[_]] = _

  protected def resetCounts() = {
    totalWeight = Double.NegativeInfinity
    allWeightsSeen = queryTargets.toList map (newWeightSeen(_))
  }

  protected def updateWeightSeenWithValue[T](value: T, weight: Double, weightSeen: WeightSeen[T]): Unit =
    weightSeen._2 += value -> logSum(weightSeen._2.getOrElse(value, Double.NegativeInfinity), weight)

  protected def updateWeightSeenForTarget[T](sample: Sample, weightSeen: WeightSeen[T]): Unit = {
    val (weight, values) = sample
    val value = values(weightSeen._1).asInstanceOf[T]
    updateWeightSeenWithValue(value, weight, weightSeen)
  }

  protected def doSample(): Unit = {
    val s = sample()    
    totalWeight = logSum(s._1, totalWeight)
    allWeightsSeen foreach (updateWeightSeenForTarget(s, _))
  }

  protected def update(): Unit = {
  }

  override protected[algorithm] def computeProjection[T](target: Element[T]): List[(T, Double)] = {
    val weightSeen = allWeightsSeen.find(_._1 == target).get._2.asInstanceOf[Map[T, Double]]
    weightSeen.mapValues(s => math.exp(s - getTotalWeight)).toList
  }
  
  def sampleFromPosterior[T](element: Element[T]): Stream[T] = {
    def nextSample(posterior: List[(Double, T)]): Stream[T] = sampleMultinomial(posterior) #:: nextSample(posterior)
    
    val elementIndex = allWeightsSeen.indexWhere(_._1 == element)
    if (elementIndex < 0) {
      throw new NotATargetException(element)
    } else {
      val (values, weights) = allWeightsSeen(elementIndex)._2.toList.unzip
      if (values.isEmpty) throw new NotATargetException(element)
      
      val logSum = logSumMany(weights)
      if (logSum == Double.NegativeInfinity) throw new ZeroTotalUnnormalizedProbabilityException
      
      val weightedValues = weights.map(w => math.exp(w - logSum)).zip(values.asInstanceOf[List[T]])
      nextSample(weightedValues)     
    }
  }
}
