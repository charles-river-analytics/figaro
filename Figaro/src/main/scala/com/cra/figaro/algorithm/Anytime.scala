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
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import akka.pattern.{ ask }
import scala.concurrent.Await
import scala.concurrent.Future
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration

/**
 * Class of services implemented by the anytime algorithm.
 */
abstract class Service

/**
 * Class of responses to services.
 */
abstract class Response

/**
 * Ack Response (String)
 */
case object AckResponse extends Response

/**
 * Exception Response (String)
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


class AnytimeAlgorithmException(s: String) extends RuntimeException(s)

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
   * A class representing the actor running the algorithm.
   */
  class Runner extends Actor {
    import context._

    def active: Receive = {
      case Handle(service) =>
        sender ! handle(service)
      case "stop" =>
        stopUpdate()
        sender ! AckResponse
        become(inactive)
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
        cleanUp()
        sender ! AckResponse
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
   * default message timeout. Increase if queries to the algorithm fail due to timeout
   */
  implicit var messageTimeout = Timeout(5000, TimeUnit.MILLISECONDS)

  /**
   * Run a single step of the algorithm. The algorithm must be able to provide answers after each step.
   */
  def runStep(): Unit

  /**
   * Optional function to run when the algorithm is stopped (not killed). Used in samplers to update lazy values.
   */
  def stopUpdate(): Unit = {}

  /**
   * A handler of services provided by the algorithm.
   */
  def handle(service: Service): Response


  protected[algorithm] def doStart() = {
    if (!running) {
      system = ActorSystem("Anytime", ConfigFactory.load(customConf))
      runner = system.actorOf(Props(new Runner))
      initialize()
      running = true
    }

    runner ! "start"
  }

  protected[algorithm] def doStop() = runner ! "stop"

  protected[algorithm] def doResume() = runner ! "resume"

  protected[algorithm] def doKill() = {
    shutdown
  }

  /**
   * Release all resources from this anytime algorithm.
   */
  def shutdown {
    if (running) {      
      awaitResponse(runner ? "kill", messageTimeout.duration)      
      system.stop(runner)
      system.shutdown
    }
  }
  
  /*
   * A helper function to query the running thread and await a response.
   * In the case that it times out, it will print a message that it timed out and return an exception response.
   * Note, on a time, it does NOT throw an exception.
   */
  protected def awaitResponse(response: Future[Any], duration: Duration): Response = {
    try {
      val result = Await.result(response, duration) 
      result match {
        case e: ExceptionResponse => {
          println(e.msg)
          e
        }
        case r: Response => r
        case _ => throw new AnytimeAlgorithmException("Unknown Response")
      }
    } catch {
      case to: TimeoutException => {
        println("Error! Did not receive a response from algorithm thread - it may be hanging or taking an exceptionally long time to respond. Try increasing messageTimeout.")
        ExceptionResponse("Timeout")
      }
      case e: Exception => throw e
    } 
  }
  
}
