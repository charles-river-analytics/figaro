/*
 * ChainFactorTest.scala 
 * ChainFactor tests.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 24, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored.factors.factory

import org.scalatest.Matchers
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec
import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.factored.factors.{ SumProductSemiring, Semiring, Variable }
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.lazyfactored.Extended
import com.cra.figaro.algorithm.factored.factors.factory.Factory

class ChainFactorTest extends WordSpec with Matchers with PrivateMethodTester {

  def findIndex[T, U](range: List[List[Any]], select: T, chain: U): Int = {
    val i = range.indexWhere(p => p(0).asInstanceOf[Regular[T]].value == select && p(1).asInstanceOf[Regular[U]].value == chain)
    i
  }
  
  def pairEqual[T, U](pair: List[Any], i0: Extended[T], i1: Extended[U]) = {
   pair(0).asInstanceOf[Regular[T]] == i0 && pair(1).asInstanceOf[Regular[U]] == i1 
  }
  
  def selectorEqual[T, U](pair: List[Any], i0: Extended[T]) = {
   pair(0).asInstanceOf[Regular[T]] == i0 
  }
  
  def chainEqual[T, U](pair: List[Any], i1: Extended[U]) = {
   pair(1).asInstanceOf[Regular[U]] == i1 
  }

  "Making factors from an element" when {

    "given a chain" should {
      "produce a selector factor over the parent variables" in {
        Universe.createNew()
        val v1 = Flip(0.2)
        val v2 = Select(0.1 -> 1, 0.9 -> 2)
        val v3 = Constant(3)
        val v4 = Chain(v1, (b: Boolean) => if (b) v2; else v3)
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v4Vals = Variable(v4).range
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)

        Variable(v1)
        Variable(v2)
        Variable(v3)
        Variable(v4)
        val factor = Factory.makeFactorsForElement(v4)
        val v4Factor = factor(0)
        val pairRange: List[List[Any]] = v4Factor.output.head.range.map(_.value.asInstanceOf[List[Any]])

        for {
          i <- v1Vals
          j <- v4Vals
          k <- v4Factor.output.head.range
        } {
          val v1ind = v1Vals indexOf (i)
          val v4ind = v4Vals indexOf (j)
          val tempind = v4Factor.output.head.range indexOf (k)
          val p = v4Factor.get(List(v1ind, v4ind, tempind))
          if (pairEqual(k.value.asInstanceOf[List[Any]], i, j)) { 
            p should equal(1.0)
          } else p should equal(0.0)
        }
        /*
  		v4Factor.get(List(v1t, 0, v41)) should equal(1.0)
        v4Factor.get(List(v1t, 0, v42)) should equal(0.0)
        v4Factor.get(List(v1t, 0, v43)) should equal(0.0)
        v4Factor.get(List(v1t, 1, v41)) should equal(0.0)
        v4Factor.get(List(v1t, 1, v42)) should equal(1.0)
        v4Factor.get(List(v1t, 1, v43)) should equal(0.0)
        v4Factor.get(List(v1t, 2, v41)) should equal(0.0)
        v4Factor.get(List(v1t, 2, v42)) should equal(0.0)
        v4Factor.get(List(v1t, 2, v43)) should equal(1.0)
        v4Factor.get(List(v1f, 3, v41)) should equal(1.0)
        v4Factor.get(List(v1f, 3, v42)) should equal(0.0)
        v4Factor.get(List(v1f, 3, v43)) should equal(0.0)
        v4Factor.get(List(v1f, 4, v41)) should equal(0.0)
        v4Factor.get(List(v1f, 4, v42)) should equal(1.0)
        v4Factor.get(List(v1f, 4, v43)) should equal(0.0)
        v4Factor.get(List(v1f, 5, v41)) should equal(0.0)
        v4Factor.get(List(v1f, 5, v42)) should equal(0.0)
        v4Factor.get(List(v1f, 5, v43)) should equal(1.0)
        * 
        */
      }

      
      
      "produce a conditional selector for each dependent element" in {
        Universe.createNew()
        val v1 = Flip(0.2)
        val v2 = Select(0.1 -> 1, 0.9 -> 2)
        val v3 = Constant(3)
        val v4 = Chain(v1, (b: Boolean) => if (b) v2; else v3)
        Values()(v4)
        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range
        val v4Vals = Variable(v4).range
        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v21 = v2Vals indexOf Regular(1)
        val v22 = v2Vals indexOf Regular(2)
        val v41 = v4Vals indexOf Regular(1)
        val v42 = v4Vals indexOf Regular(2)
        val v43 = v4Vals indexOf Regular(3)

        Variable(v1)
        Variable(v2)
        Variable(v3)
        Variable(v4)
        val factor = Factory.makeFactorsForElement(v4)
        
        for {
          k <- 0 until v1Vals.size
          i <- 0 until factor(k+1).parents.head.size  
          j <- 0 until (if (v1Vals(k).value) v2Vals.size else 1)
        } {
          val f = factor(k+1)
          val p = f.get(List(i, j))
          val vals = if (v1Vals(k).value) v2Vals else  Variable(v3).range
           if (selectorEqual(f.parents.head.range(i).value.asInstanceOf[List[Any]], v1Vals(k))) { 
            if (chainEqual(f.parents.head.range(i).value.asInstanceOf[List[Any]], vals(j))) p should equal(1.0)
            else p should equal(0.0)
          } else p should equal(1.0)
        }
        
        /*
        v2Factor.get(List(0, v21)) should equal(1.0)
        v2Factor.get(List(0, v22)) should equal(0.0)
        v2Factor.get(List(1, v21)) should equal(0.0)
        v2Factor.get(List(1, v22)) should equal(1.0)
        v2Factor.get(List(2, v21)) should equal(0.0)
        v2Factor.get(List(2, v22)) should equal(0.0)

        // Don't cares
        v2Factor.get(List(3, v21)) should equal(1.0)
        v2Factor.get(List(3, v22)) should equal(1.0)
        v2Factor.get(List(4, v21)) should equal(1.0)
        v2Factor.get(List(4, v22)) should equal(1.0)
        v2Factor.get(List(5, v21)) should equal(1.0)
        v2Factor.get(List(5, v22)) should equal(1.0)


        val v3Factor = factor(2)

        v3Factor.get(List(3, 0)) should equal(0.0)
        v3Factor.get(List(4, 0)) should equal(0.0)
        v3Factor.get(List(5, 0)) should equal(1.0)

        // Don't cares
        v3Factor.get(List(0, 0)) should equal(1.0)
        v3Factor.get(List(1, 0)) should equal(1.0)
        v3Factor.get(List(2, 0)) should equal(1.0)
        * 
        */

      }
    }

    /* CPDs are chains, these tests are not needed */
    /*
    "given a CPD with one argument" should {
      "produce a selector factor over the parent variables" in {
        Universe.createNew()
        val v1 = Flip(0.2)

        val v2 = CPD(v1, false -> Flip(0.1), true -> Flip(0.7))
        Values()(v2)

        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range

        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v2t = v2Vals indexOf Regular(true)
        val v2f = v2Vals indexOf Regular(false)
        val v3t = 0
        val v3f = 1
        val v4t = 0
        val v4f = 1

        val factor = Factory.makeFactorsForElement(v2)
        val v2Factor = factor(0)

        v2Factor.get(List(v1t, 0, v2t)) should equal(1.0)
        v2Factor.get(List(v1t, 0, v2f)) should equal(0.0)
        v2Factor.get(List(v1t, 1, v2t)) should equal(0.0)
        v2Factor.get(List(v1t, 1, v2f)) should equal(1.0)
        v2Factor.get(List(v1t, 2, v2t)) should equal(0.0)
        v2Factor.get(List(v1t, 2, v2f)) should equal(0.0)
        v2Factor.get(List(v1t, 3, v2t)) should equal(0.0)
        v2Factor.get(List(v1t, 3, v2f)) should equal(0.0)
        v2Factor.get(List(v1f, 0, v2t)) should equal(0.0)
        v2Factor.get(List(v1f, 0, v2f)) should equal(0.0)
        v2Factor.get(List(v1f, 1, v2t)) should equal(0.0)
        v2Factor.get(List(v1f, 1, v2f)) should equal(0.0)
        v2Factor.get(List(v1f, 2, v2t)) should equal(1.0)
        v2Factor.get(List(v1f, 2, v2f)) should equal(0.0)
        v2Factor.get(List(v1f, 3, v2t)) should equal(0.0)
        v2Factor.get(List(v1f, 3, v2f)) should equal(1.0)
      }

      "produce a conditional selector for each dependent element" in {
        Universe.createNew()
        val v1 = Flip(0.2)

        val v2 = CPD(v1, false -> Flip(0.1), true -> Flip(0.7))
        Values()(v2)

        val v1Vals = Variable(v1).range
        val v2Vals = Variable(v2).range

        val v1t = v1Vals indexOf Regular(true)
        val v1f = v1Vals indexOf Regular(false)
        val v2t = v2Vals indexOf Regular(true)
        val v2f = v2Vals indexOf Regular(false)
        val v3t = 0
        val v3f = 1
        val v4t = 0
        val v4f = 1

        val factor = Factory.makeFactorsForElement(v2)
        val v3Factor = factor(1)

        v3Factor.get(List(0, v3t)) should equal(1.0)
        v3Factor.get(List(0, v3f)) should equal(0.0)
        v3Factor.get(List(1, v3t)) should equal(0.0)
        v3Factor.get(List(1, v3f)) should equal(1.0)

        // Don't cares
        v3Factor.get(List(2, v3t)) should equal(1.0)
        v3Factor.get(List(2, v3f)) should equal(1.0)
        v3Factor.get(List(3, v3t)) should equal(1.0)
        v3Factor.get(List(3, v3f)) should equal(1.0)

        val v4Factor = factor(2)

        v4Factor.get(List(2, v4t)) should equal(1.0)
        v4Factor.get(List(2, v4f)) should equal(0.0)
        v4Factor.get(List(3, v4t)) should equal(0.0)
        v4Factor.get(List(3, v4f)) should equal(1.0)

        // Don't cares
        v4Factor.get(List(0, v4t)) should equal(1.0)
        v4Factor.get(List(0, v4f)) should equal(1.0)
        v4Factor.get(List(1, v4t)) should equal(1.0)
        v4Factor.get(List(1, v4f)) should equal(1.0)

      }
    }
    * 
    * 
    */
  }

}
