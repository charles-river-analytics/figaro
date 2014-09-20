/*
 * ValveReliability.scala
 * A Factored Frontier implementation, based on the valve example from Weber and Jouffe, 2003.
 * 
 * Created By:      William Kretschmer (kretsch@mit.edu), Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Jul 22, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.language._
import com.cra.figaro.algorithm.filtering.FactoredFrontier

/**
 * A Factored Frontier implementation, based on the valve example from Weber and Jouffe, 2003.
 */
object ValveReliability {
  trait ValveState
  case object OK extends ValveState  // A functioning valve
  case object RO extends ValveState  // A valve that remains open
  case object RC extends ValveState  // A valve that remains closed
  
  // The initial state of each of the valves
  val initial = Universe.createNew()
  Constant(OK)("v1", initial)
  Constant(OK)("v2", initial)
  Constant(OK)("v3", initial)
  
  // Elements representing the valves' rates of failure
  val static = Universe.createNew()
  Select(0.997 -> OK, 0.002 -> RO, 0.001 -> RC)("f1", static)
  Select(0.995 -> OK, 0.003 -> RO, 0.002 -> RC)("f2", static)
  Select(0.993 -> OK, 0.004 -> RO, 0.003 -> RC)("f3", static)
  
  // If a valve is OK, we model with the possibility of failure
  // Otherwise, the valve will remain in a failed state
  val failure = (v: ValveState, f: ValveState) => if(v == OK) f else v
  
  val transition = (static: Universe, previous: Universe) => {
    val v1prev = previous.getElementByReference[ValveState]("v1")
    val v2prev = previous.getElementByReference[ValveState]("v2")
    val v3prev = previous.getElementByReference[ValveState]("v3")
    val f1 = static.getElementByReference[ValveState]("f1")
    val f2 = static.getElementByReference[ValveState]("f2")
    val f3 = static.getElementByReference[ValveState]("f3")
    
    val next = Universe.createNew()
    val v1 = Apply(v1prev, f1, failure)("v1", next)
    val v2 = Apply(v2prev, f2, failure)("v2", next)
    val v3 = Apply(v3prev, f3, failure)("v3", next)
    
    // The probabilities that the system remains open, closed, or controllable at a given time step
    val open = Apply(v1, v2, v3, (v1: ValveState, v2: ValveState, v3: ValveState) => v1 == RO && (v2 == RO || v3 == RO))("open", next)
    val closed = Apply(v1, v2, v3, (v1: ValveState, v2: ValveState, v3: ValveState) => v1 == RC || (v2 == RC && v3 == RC))("closed", next)
    val controllable = Apply(v1, v2, v3, (v1: ValveState, v2: ValveState, v3: ValveState) => !((v1 == RO && (v2 == RO || v3 == RO)) || (v1 == RC || (v2 == RC && v3 == RC))))("controllable", next)
    next
  }
  
  def main(args: Array[String]) {
    val ff = FactoredFrontier(static, initial, transition, 5)
    ff.start()
    println("|------------|------------|------------|------------|")
    println("|Iteration   |Open        |Closed      |Controllable|")
    println("|------------|------------|------------|------------|")
    println("|           0|      0.0000|      0.0000|      1.0000|")
    for(i <- 1 to 2000){
      ff.advanceTime()
      if(i % 50 == 0) {
        printf("|%12s|", i)
        printf("      %.6s|", ff.currentProbability("open", true))
        printf("      %.6s|", ff.currentProbability("closed", true))
        printf("      %.6s|%n", ff.currentProbability("controllable", true))
      }
    }
    println("|------------|------------|------------|------------|")
    ff.kill()
  }
}