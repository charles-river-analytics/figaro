/*
 * Algorithm.scala
 * General Figaro algorithms.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm

/**
 * Algorithm.scala
 *
 * General class of Figaro algorithms.
 */

import com.cra.figaro.language._

class UnsupportedAlgorithmException(element: Element[_]) extends RuntimeException(element.toString)

class AlgorithmException extends RuntimeException
class AlgorithmInactiveException extends AlgorithmException
class AlgorithmActiveException extends AlgorithmException

/**
 * The general class of Figaro algorithms. The Algorithm class is defined to generalize both
 * one-time and anytime algorithms, using a start/stop/resume/kill mechanism.
 */
trait Algorithm {
  /**
   * Called when the algorithm is started before running any steps. By default, does nothing. Can be overridden.
   */
  def initialize(): Unit = {}

  /**
   * Called when the algorithm is killed. By default, does nothing. Can be overridden.
   */
  def cleanUp(): Unit = {}

  /* The following six methods are implemented by AnytimeAlgorithm and OneTimeAlgorithm. */
  /*
   * Start the algorithm. After it returns, the algorithm must be ready to provide answers.
   */
  protected[algorithm] def doStart(): Unit

  /*
   * Stop the algorithm from computing. The algorithm is still ready to provide answers after it returns.
   */
  protected[algorithm] def doStop(): Unit

  /*
   * Resume the computation of the algorithm, if it has been stopped.
   */
  protected[algorithm] def doResume(): Unit

  /*
   * Kill the algorithm so that it is inactive. It will no longer be able to provide answers.
   */
  protected[algorithm] def doKill(): Unit

  protected var active = false

  def isActive = active

  /*
   * The following are convenient wrappers for the Algorithm's methods that do some error checking before 
   * calling the appropriate method.
   */

  /**
   * Start the algorithm and make it active. After it returns, the algorithm must be ready to provide
   * answers. Throws AlgorithmActiveException if the algorithm is already active.
   */

  def start(): Unit = {
    if (active) throw new AlgorithmActiveException
    active = true
    doStart()
  }

  /**
   * Stop the algorithm from computing. The algorithm is still ready to provide answers after it returns.
   * Throws AlgorithmInactiveException if the algorithm is not active.
   */
  def stop(): Unit = {
    if (!active) throw new AlgorithmInactiveException
    doStop()
  }

  /**
   * Resume the computation of the algorithm, if it has been stopped. Throws AlgorithmInactiveException if
   * the algorithm is not active.
   */
  def resume(): Unit = {
    if (!active) throw new AlgorithmInactiveException
    doResume()
  }

  /**
   * Kill the algorithm so that it is inactive. It will no longer be able to provide answers.Throws
   * AlgorithmInactiveException if the algorithm is not active.
   */
  def kill(): Unit = {
    if (!active) throw new AlgorithmInactiveException
    doKill()
    active = false
  }

}
