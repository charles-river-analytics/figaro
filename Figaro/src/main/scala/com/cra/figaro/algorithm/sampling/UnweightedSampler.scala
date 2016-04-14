/*
 * UnweightedSampler.scala
 * Unweighted samplers.
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

import scala.collection.mutable.Map
import scala.language.existentials
import scala.language.postfixOps

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._

/**
 * Samplers that use samples without weights.
 */
abstract class BaseUnweightedSampler(val universe: Universe, targets: Element[_]*) extends Sampler {
  lazy val queryTargets = targets.toList
  /**
   * A sample is a map from elements to their values.
   */
  type Sample = Map[Element[_], Any]

  /**
   * Produce a single sample.
   */
  def sample(): (Boolean, Sample)

  protected var sampleCount: Int = _

  /**
   * Number of samples taken
   */
  def getSampleCount = sampleCount

  // To save memory, we do not store the entire list of samples. Rather, we compile, for each target, how many times
  // each value of the target has been seen. If the same values appear many times for a target, considerable space
  // will be saved. However, if every sample produces a different value for the target, the memory requirements will
  // still be substantial.

  protected type TimesSeen[T] = Map[T, Int]
  protected type LastUpdate[T] = (T, Int)

  protected def newTimesSeen[T](target: Element[T]): TimesSeen[T] = Map()
  protected def newLastUpdate[T](target: Element[T]): LastUpdate[T] = (target.value, 1)

  protected var allTimesSeen: Map[Element[_], TimesSeen[_]] = Map()

  protected var allLastUpdates: Map[Element[_], LastUpdate[_]] = Map()

  protected def initUpdates() = allLastUpdates = Map(targets.toList.map(t => (t -> newLastUpdate(t))): _*)

  protected def resetCounts() {
    sampleCount = 0
    allTimesSeen = Map(targets.toList.map(t => (t -> newTimesSeen(t))): _*)
  }

  protected def updateTimesSeenWithValue[T](value: T, timesSeen: TimesSeen[T], seen: Int): Unit =
    timesSeen += value -> (timesSeen.getOrElse(value, 0) + seen)

  protected def updateTimesSeenForTarget[T](elem: Element[T], newValue: T): Unit = {
    val lastVal = allLastUpdates(elem).asInstanceOf[LastUpdate[T]]
    updateTimesSeenWithValue(lastVal._1, allTimesSeen(elem).asInstanceOf[TimesSeen[T]], sampleCount - lastVal._2)
    allLastUpdates += (elem -> (newValue, sampleCount))
  }

  protected def doSample(): Unit = {
    val s = sample()
    if (sampleCount == 0 && s._1) {
      initUpdates
    }
    if (s._1) {
      sampleCount += 1
      s._2 foreach (t => updateTimesSeenForTarget(t._1.asInstanceOf[Element[t._1.Value]], t._2.asInstanceOf[t._1.Value]))
    }
  }

  protected def update(): Unit = {
    sampleCount += 1
    if (allLastUpdates.nonEmpty) targets.foreach(t => updateTimesSeenForTarget(t.asInstanceOf[Element[t.Value]], t.value))
    sampleCount -= 1
  }

}

trait UnweightedSampler extends BaseUnweightedSampler with ProbQuerySampler with StreamableProbQueryAlgorithm {

  /**
   * Total weight of samples taken, in log space
   */
  def getTotalWeight: Double = math.log(getSampleCount)

  override protected[algorithm] def computeProjection[T](target: Element[T]): List[(T, Double)] = {
    if (allLastUpdates.nonEmpty) {
      val timesSeen = allTimesSeen.find(_._1 == target).get._2.asInstanceOf[Map[T, Int]]
      timesSeen.mapValues(_ / sampleCount.toDouble).toList
    } else {
      println("Error: MH sampler did not accept any samples")
      List()
    }
  }
  
  def sampleFromPosterior[T](element: Element[T]): Stream[T] = {
    def nextSample(posterior: List[(Double, T)]): Stream[T] = sampleMultinomial(posterior) #:: nextSample(posterior)

    if (!allTimesSeen.contains(element)) {
      throw new NotATargetException(element)
    } else {
      val (values, weights) = allTimesSeen(element).toList.unzip
      if (values.isEmpty) throw new NotATargetException(element)

      if (sampleCount == 0) throw new ZeroTotalUnnormalizedProbabilityException

      val weightedValues = weights.map(w => w.toDouble/sampleCount.toDouble).zip(values.asInstanceOf[List[T]])
      nextSample(weightedValues)
    }
  }

}
