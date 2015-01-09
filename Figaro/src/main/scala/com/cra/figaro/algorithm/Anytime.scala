/*
 * Anytime.scala
 * Anytime algorithms
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

import com.cra.figaro.language._
import akka.actor._
import com.typesafe.config.ConfigFactory

/**
 * Class of services implemented by the anytime algorithm.
 */
abstract class Service

/**
 * Class of responses to services.
 */
abstract class Response
/**
 * General Response (String)
 */
case class ExceptionResponse(msg: String) extends Response

/**
 * Messages to or from the actor.
 */
sealed abstract class Message
/**
 * A message to the handler to handle the given service.
 */
case class Handle(service: Service) extends Message

/**
 * An anytime algorithm is able to improve its estimated answers over time. Anytime algorithms run in their
 * own thread using an actor.
 *
 * An anytime algorithm must implement initialize, runStep, and handle methods. runStep will typically send a
 * Handle message to the actor containing a Service, and the algorithm will provide a handle method to
 * implement this Service. The handle method will return a Response, which is sent back to runStep.
 */

trait Anytime extends Algorithm {
  /**
   * Run a single step of the algorithm. The algorithm must be able to provide answers after each step.
   */
  def runStep(): Unit

  /**
   * Optional function to run when the algorithm is stopped (not killed). Used in samplers to update lazy values.
   */
  def stopUpdate(): Unit = {  }

  /**
   * A class representing the actor running the algorithm.
   */
  class Runner extends Actor {
    import context._

    def active: Receive = {
      case Handle(service) =>
        sender ! handle(service)
      case "stop" =>
        stopUpdate()
        become (inactive)
      case "next" =>
        runStep()
        self ! "next"
      case _ =>
        sender ! ExceptionResponse("Algorithm is still running")
    }

    def inactive: Receive = {
      case Handle(service) =>
        sender ! handle(service)
      case "start" =>
        runStep()
        become(active)
        self ! "next"
      case "resume" =>
        resume()
        become(active)
      	self ! "next"
      case "kill" =>
         become(shuttingDown)
      case _ =>
        sender ! ExceptionResponse("Algorithm is stopped")
    }

    def shuttingDown: Receive = {
      case _ =>
        sender ! ExceptionResponse("Anytime algorithm has terminated")
    }

    def receive = inactive


  }

  /**
   * The actor running the algorithm.
   */
  val customConf = ConfigFactory.parseString("""
		  akka {
		     log-dead-letters = 0
		     log-dead-letters-during-shutdown = off

		  }
		  """)

  var system: ActorSystem = null
  var runner: ActorRef = null
  var running = false;

  /**
   * A handler of services provided by the algorithm.
   */
  def handle(service: Service): Response


  protected def doStart() = {
    if (!running) {
    	system = ActorSystem("Anytime", ConfigFactory.load(customConf))
    	runner = system.actorOf(Props(new Runner))
    	initialize()
    	running = true
    }

    runner ! "start"
  }

  protected def doStop() = runner ! "stop"

  protected def doResume() = runner ! "resume"

  protected def doKill() = {
    shutdown
  }

  /**
   * Release all resources from this anytime algorithm.
   */
  def shutdown {
    cleanUp()
    if (running)
    {
    	runner ! "kill"
    	system.stop(runner)
    	system.shutdown
    }
  }
}
