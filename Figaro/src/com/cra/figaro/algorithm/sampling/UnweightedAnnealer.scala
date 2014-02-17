/*
 * UnweightedAnnealer.scala
 * Unweighted Annealing samplers.
 * 
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Mar 1, 2013
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
import math.log

/**
 * Samplers that use samples without weights.
 */
abstract class UnweightedAnnealer(override val universe: Universe, targets: Element[_]*)
  extends MPEAlgorithm with Sampler {

  /**
   * Anneal the model with the provided iteration count.
   */
  def sample(iter: Int): (Boolean, Double)

  protected var sampleCount: Int = _

  /**
   * Number of samples taken.
   */
  def getSampleCount = sampleCount

  protected var bestState: Map[Element[_], Any] = _
  protected var bestEnergy: Double = _
  protected var currentEnergy: Double = _

  protected def resetCounts() {
    sampleCount = 0
    bestState = Map()
    bestEnergy = Double.MinValue
  }

  protected def computeEnergy = universe.constrainedElements.foldLeft(0.0)((c: Double, e: Element[_]) => c + log(e.constraintValue))

  private def saveState = {
    universe.activeElements foreach (e => bestState += (e -> e.value))
    bestEnergy = currentEnergy
  }

  protected def doSample(): Unit = {
    val (validState, energyUpdate) = sample(sampleCount)
    if (sampleCount == 0) {
      currentEnergy = computeEnergy
    } else {
      currentEnergy += log(energyUpdate)
    }
    if (validState) {
      sampleCount += 1
      if (currentEnergy > bestEnergy) saveState
    }
  }

  protected def update(): Unit = {}

  override def mostLikelyValue[T](target: Element[T]): T = {
    bestState(target).asInstanceOf[T]
  }

  /**
   * Return the best energy computed by the annealer.
   */
  def getBestEnergy = bestEnergy
  /**
   * Return the current energy of the annealer.
   */
  def getCurrentEnergy = currentEnergy

}
