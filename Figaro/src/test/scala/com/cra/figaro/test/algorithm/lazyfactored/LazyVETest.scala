/*
 * LazyVETest.scala
 * Lazy variable elimination tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 21, 2013
 * 
 * Copyright 2017 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.lazyfactored

import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.lazyfactored.LazyVariableElimination

class LazyVETest extends WordSpec with Matchers {
  "Running lazy variable elimination" when {
    "given a simple two-level chain with no evidence" should {
      "produce the correct depth 1 approximation" in {
    	Universe.createNew()
    	val outerIf = twoLevelChain
        val alg = new LazyVariableElimination(outerIf)
    	alg.start()
    	alg.probabilityBounds(outerIf, true)._1 should be (0.02 +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._1 should be (0.08 +- 0.000000001)
    	alg.probabilityBounds(outerIf, true)._2 should be (0.92 +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._2 should be (0.98 +- 0.000000001)
      }
      
      "produce the correct depth 2 perfect answer" in {
    	Universe.createNew()
    	val outerIf = twoLevelChain
        val alg = new LazyVariableElimination(outerIf)
    	alg.start()
    	alg.pump()
    	val pInner = 0.3 * 0.4 + 0.7 * 0.5
    	val pOuter = 0.1 * 0.2 + 0.9 * pInner
    	alg.probabilityBounds(outerIf, true)._1 should be (pOuter +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._1 should be ((1 - pOuter) +- 0.000000001)
    	alg.probabilityBounds(outerIf, true)._2 should be (pOuter +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._2 should be ((1 - pOuter) +- 0.000000001)
      
      alg.kill()
      } 
    }

    "given a simple two-level chain with a condition" should {
      "produce the correct depth 2 approximation" in {
    	Universe.createNew()
    	// This is designed so that depth 2 catches the check and its Flip arguments, but not the inner Flips of the two level chain.
    	val outerIf = twoLevelChain
    	val apply = Apply(outerIf, (b: Boolean) => b)
    	val check = If(apply, Flip(0.6), Flip(0.3))
    	check.observe(true)
        val alg = new LazyVariableElimination(apply)
    	alg.start()
    	alg.pump()
    	val trueWeight = 0.1 * 0.2 * 0.6
    	val falseWeight = 0.1 * 0.8 * 0.3
    	val starWeightLower = 0
    	val starWeightUpper = 0.9
    	val lowerZ = trueWeight + falseWeight + starWeightLower
    	val upperZ = trueWeight + falseWeight + starWeightUpper
    	val trueLower = trueWeight / upperZ
    	val falseLower = falseWeight / upperZ
    	alg.probabilityBounds(apply, true)._1 should be (trueLower +- 0.000000001)
    	alg.probabilityBounds(apply, false)._1 should be (falseLower +- 0.000000001)
    	alg.probabilityBounds(apply, true)._2 should be ((1 - falseLower) +- 0.000000001)
    	alg.probabilityBounds(apply, false)._2 should be ((1 - trueLower) +- 0.000000001)
      
      alg.kill()
      }
      
      "produce the correct depth 3 perfect answer" in {
    	Universe.createNew()
    	val outerIf = twoLevelChain
    	val apply = Apply(outerIf, (b: Boolean) => b)
    	val check = If(apply, Flip(0.6), Flip(0.3))
    	check.observe(true)
        val alg = new LazyVariableElimination(apply)
    	alg.start()
    	alg.pump()
    	alg.pump()
    	val pInner = 0.3 * 0.4 + 0.7 * 0.5 
    	val pOuterT = 0.1 * 0.2 + 0.9 * pInner
    	val pOuterF = 1.0 - pOuterT
    	val qOuterT = pOuterT * 0.6
    	val qOuterF = pOuterF * 0.3
    	val pOuter = qOuterT / (qOuterF + qOuterT)
    	alg.probabilityBounds(apply, true)._1 should be (pOuter +- 0.000000001)
    	alg.probabilityBounds(apply, false)._1 should be ((1 - pOuter) +- 0.000000001)
    	alg.probabilityBounds(apply, true)._2 should be (pOuter +- 0.000000001)
    	alg.probabilityBounds(apply, false)._2 should be ((1 - pOuter) +- 0.000000001)
      
      alg.kill()
      } 
    }

    "given a simple two-level chain with a constraint" should {
      "produce the correct depth 1 approximation" in {
    	Universe.createNew()
    	val outerIf = twoLevelChain
    	outerIf.addConstraint((b: Boolean) => if (b) 0.6; else 0.3)
    	val alg = new LazyVariableElimination(outerIf)
    	alg.start()
    	val trueWeight = 0.1 * 0.2 * 0.6
    	val falseWeight = 0.1 * 0.8 * 0.3
    	val starWeightLower = 0
    	val starWeightUpper = 0.9
    	val lowerZ = trueWeight + falseWeight + starWeightLower
    	val upperZ = trueWeight + falseWeight + starWeightUpper
    	val trueLower = trueWeight / upperZ
    	val falseLower = falseWeight / upperZ
    	alg.probabilityBounds(outerIf, true)._1 should be (trueLower +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._1 should be (falseLower +- 0.000000001)
    	alg.probabilityBounds(outerIf, true)._2 should be ((1 - falseLower) +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._2 should be ((1 - trueLower) +- 0.000000001)
      
      alg.kill()
      }
      
      "produce the correct depth 2 perfect answer" in {
    	Universe.createNew()
    	val outerIf = twoLevelChain
    	outerIf.addConstraint((b: Boolean) => if (b) 0.6; else 0.3)
        val alg = new LazyVariableElimination(outerIf)
    	alg.start()
    	alg.pump()
    	val pInner = 0.3 * 0.4 + 0.7 * 0.5 
    	val pOuterT = 0.1 * 0.2 + 0.9 * pInner
    	val pOuterF = 1.0 - pOuterT
    	val qOuterT = pOuterT * 0.6
    	val qOuterF = pOuterF * 0.3
    	val pOuter = qOuterT / (qOuterF + qOuterT)
    	alg.probabilityBounds(outerIf, true)._1 should be (pOuter +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._1 should be ((1 - pOuter) +- 0.000000001)
    	alg.probabilityBounds(outerIf, true)._2 should be (pOuter +- 0.000000001)
    	alg.probabilityBounds(outerIf, false)._2 should be ((1 - pOuter) +- 0.000000001)
      
      alg.kill()
      } 
    }
    
    "given a lazy list with no condition" should {
      "produce a shrinking sequence of bounds converging on the correct answer" in {
        Universe.createNew()
        val el = generate()
        val cb = contains('b, el)
        val ca = contains('a, el)
        val alg = new LazyVariableElimination(cb)
        alg.start()
        var (pLow, pHigh) = alg.probabilityBounds(cb, true)
        for { i <- 2 to 20 } {
          alg.pump()
          val (newPLow, newPHigh) = alg.probabilityBounds(cb, true)
          newPLow should be >= (pLow - 0.00000001)
          newPHigh should be <= (pHigh + 0.000000001)
          pLow = newPLow
          pHigh = newPHigh
        }
        val correct = 2.0 / 7.0
        pLow should be <= (correct)
        pLow should be >= (correct - 0.0001)
        pHigh should be >= (correct)
        pHigh should be <= (correct + 0.0001)
        
        alg.kill()
      }
    }

    "given a lazy list with a condition" should {
      "produce a shrinking sequence of bounds converging on the correct answer" in {
        Universe.createNew()
        val el = generate()
        val cb = contains('b, el)
        val ca = contains('a, el)
        ca.observe(true)
        val alg = new LazyVariableElimination(cb)
        alg.start()
        var (pLow, pHigh) = alg.probabilityBounds(cb, true)
        for { i <- 2 to 20 } {
          alg.pump()
          val (newPLow, newPHigh) = alg.probabilityBounds(cb, true)
          newPLow should be >= (pLow - 0.00000001)
          newPHigh should be <= (pHigh + 0.000000001)
          pLow = newPLow
          pHigh = newPHigh
        }
        val correct = 3.0 / 7.0
        pLow should be <= (correct)
        pLow should be >= (correct - 0.0001)
        pHigh should be >= (correct)
        pHigh should be <= (correct + 0.0001)
        
        alg.kill()
      }
    }
  }

  /* Two-level chain model */
  def twoLevelChain() = {
    val fl1 = Flip(0.1)
    val fl2 = Flip(0.2)
    val fl3 = Flip(0.3)
    val fl4 = Flip(0.4)
    val fl5 = Flip(0.5)
    val innerIf = If(fl3, fl4, fl5)
    If(fl1, fl2, innerIf)
  }

  /* Lazy list model */
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

}