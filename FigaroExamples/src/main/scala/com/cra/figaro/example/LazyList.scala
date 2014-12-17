/*
 * LazyList.scala
 * A lazy list.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 26, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.lazyfactored.LazyVariableElimination
import com.cra.figaro.algorithm.lazyfactored.LazyValues

object LazyList {
  val universe = Universe.createNew()
  
  class L
  case object Empty extends L
  case class Cons(head: Element[Symbol], tail: Element[L]) extends L
  
  def contains(target: Symbol, el: Element[L]): Element[Boolean] = {
    Chain(el, (l: L) => {
      l match {
        case Empty => Constant(false)
        case Cons(head, tail) => If(head === target, Constant(true), contains(target, tail))
      }
    })
  }

  def generate(): Element[L] = {
    Apply(Flip(0.5), (b: Boolean) => if (b) Empty; else Cons(Select(0.6 -> 'a, 0.4 -> 'b), generate()))
  }
  
  def main(args: Array[String]) {
    
    val el = generate()
    val cb = contains('b, el)
    val ca = contains('a, el)
    ca.observe(true)
    val alg = new LazyVariableElimination(cb)
    
    println("DEPTH " + 1)
    alg.start()
    println(alg.currentResult.toReadableString)
    //alg.debug = true
    //LazyValues.debug = true
    for { i <- 2 to 20 } {
      println("DEPTH " + i)
      alg.pump()
      println(alg.currentResult.toReadableString)
    }
    println("Correct probability of true is " + (3.0 / 7.0))
  }

}