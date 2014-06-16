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
 * Message to start the anytime algorithm
 */
//case object Start extends Message
/**
 * Message to stop the anytime algorithm
 */
//case object Stop extends Message
/**
 * Message to resume the anytime algorithm
 */
//case object Resume extends Message
/**
 * Message to kill the anytime algorithm
 */
//case object Kill extends Message

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
   * Optional function to run when the algorithm is stopped (not killed). Used in samplers to update lazy values
   */
  def stopUpdate(): Unit = {  }

  /**
   * A class representing the actor running the algorithm.
   */
  class Runner extends Actor {
    import context._
    
    def active: Receive = {
      case "stop" => 
        become (inactive)
        stopUpdate()
      case "next" => 
        runStep()
        self ! "next"
      case Kill => 
         become(killed)
         kill()
         cleanUp()
      case _ => 
        sender ! ExceptionResponse("Algorithm is still running")
    }

    def inactive: Receive = {
      case Handle(service) => sender ! handle(service)
      case "start" => become(active)
        self ! "next"
      case "resume" => become(active)
      	self ! "next"
      case _ => 
        sender ! ExceptionResponse("Algorithm is stopped")
    }
    
    def killed: Receive = {
      case _ => 
        sender ! ExceptionResponse("Algorithm has terminated")
    }
    
    def receive = inactive
//    
//    def act() {
//      running = true
//      while (true) {
//        if (running) runStep()
//        receiveWithin(0) {
//          case TIMEOUT => ()
//          case Stop => { running = false; stopUpdate() }
//          case Resume => running = true
//          case Kill =>
//            exit()
//          case Handle(service) => sender ! handle(service)
//        }
//      }
//    }
  }

  /**
   * The actor running the algorithm.
   */
  protected var runner: ActorRef = _

  /**
   * A handler of services provided by the algorithm.
   */
  def handle(service: Service): Response

  val system = ActorSystem()
  
  protected def doStart() = {
    initialize()
    
    runner = system.actorOf(Props[Runner], "Anytime")
    runner ! "start"
  }

  protected def doStop() = runner ! "stop"

  protected def doResume() = runner ! "resume"

  protected def doKill() = runner ! "kill"
}
