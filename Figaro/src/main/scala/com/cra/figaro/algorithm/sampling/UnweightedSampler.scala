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

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import scala.collection.mutable.Map
import scala.language.existentials
import scala.language.postfixOps

/**
 * Samplers that use samples without weights.
 */
abstract class UnweightedSampler(override val universe: Universe, targets: Element[_]*) extends ProbQueryAlgorithm with Sampler {
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
   * Number of samples taken.
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

  protected var allTimesSeen: Map[Element[_], TimesSeen[_]] = _

  protected var allLastUpdates: Map[Element[_], LastUpdate[_]] = _

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
    if (sampleCount == 0) {
      initUpdates
    }
    if (s._1) {
      sampleCount += 1
      s._2 foreach (t => updateTimesSeenForTarget(t._1.asInstanceOf[Element[t._1.Value]], t._2.asInstanceOf[t._1.Value]))
    }
  }

  protected def update(): Unit = {
    sampleCount += 1
    targets.foreach(t => updateTimesSeenForTarget(t.asInstanceOf[Element[t.Value]], t.value))
    sampleCount -= 1
  }

  private def projection[T](target: Element[T]): List[(T, Double)] = {
    val timesSeen = allTimesSeen.find(_._1 == target).get._2.asInstanceOf[Map[T, Int]]
    timesSeen.mapValues(_ / sampleCount.toDouble).toList
  }

  /**
   * Return an estimate of the expectation of the function under the marginal probability distribution
   * of the target.
   */
  def computeExpectation[T](target: Element[T], function: T => Double) = {
    val contributions = projection(target) map (pair => function(pair._1) * pair._2)
    (0.0 /: contributions)(_ + _)
  }

  /**
   * Return an estimate of the marginal probability distribution over the target that lists each element
   * with its probability.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] =
    projection(target) map (_.swap) toStream
}
