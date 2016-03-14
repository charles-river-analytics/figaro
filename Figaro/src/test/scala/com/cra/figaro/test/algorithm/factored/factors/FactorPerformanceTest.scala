/*
 * FactorPerformanceTest.scala 
 * Factor tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored.factors

import org.scalatest.Matchers
import org.scalatest.PrivateMethodTester
import org.scalatest.WordSpec
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.library.atomic.discrete.FromRange
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored.LazyValues
import com.cra.figaro.language.Element.toIntElement
import scala.reflect.runtime.universe
import com.cra.figaro.algorithm.factored.factors.factory.Factory

class FactorPerformanceTest extends WordSpec with Matchers with PrivateMethodTester {

  val factorSize = 75
  val analyze = true

  "A sparse factor" should {
    "multiply O(n^k) faster than basic factors" in {
      Universe.createNew()
      val p1 = FromRange(0, factorSize)
      val p2 = FromRange(0, factorSize)
      val sum = p1 ++ p2
      LazyValues(Universe.universe).expandAll(Set((p1, Int.MaxValue), (p2, Int.MaxValue), (sum, Int.MaxValue)))
      val p1v = Variable(p1)
      val p2v = Variable(p2)
      val sumv = Variable(sum)

      val p1Factor = Factory.makeFactorsForElement(p1).head
      val p2Factor = Factory.makeFactorsForElement(p2).head
      val sumSparseFactor = Factory.makeFactorsForElement(sum).head

      val sumBasicFactor = new BasicFactor[Double](sumSparseFactor.parents, sumSparseFactor.output)
      val arg1Indices = sumSparseFactor.parents(0).range.zipWithIndex
      val arg2Indices = sumSparseFactor.parents(1).range.zipWithIndex
      val resultIndices = sumv.range.zipWithIndex
      for {
        (arg1Val, arg1Index) <- arg1Indices
        (arg2Val, arg2Index) <- arg2Indices
        (resultVal, resultIndex) <- resultIndices
      } {
        sumBasicFactor.set(List(arg1Index, arg2Index, resultIndex), 0.0)
      }
      sumSparseFactor.getIndices.foreach(f => sumBasicFactor.set(f, sumSparseFactor.get(f)))

      val sparseTime = measureTime(() => sumSparseFactor.product(p1Factor/*, SumProductSemiring*/), 3, 5)
      val denseTime = measureTime(() => sumBasicFactor.product(p1Factor/*, SumProductSemiring*/), 3, 5)

      if (analyze) {

        println("Sparse factor size: " + sumSparseFactor.contents.size)
        println("Basic factor size: " + sumBasicFactor.contents.size)

        println("Performance ratio (dense/sparse) = " + denseTime / sparseTime)
      }

      denseTime / sparseTime should be >= 5.0
    }

    "sumOver O(n^k) faster than basic factors" in {
      Universe.createNew()
      val p1 = FromRange(0, factorSize)
      val p2 = FromRange(0, factorSize)
      val sum = p1 ++ p2
      LazyValues(Universe.universe).expandAll(Set((p1, Int.MaxValue), (p2, Int.MaxValue), (sum, Int.MaxValue)))
      val p1v = Variable(p1)
      val p2v = Variable(p2)
      val sumv = Variable(sum)

      val sumSparseFactor = Factory.makeFactorsForElement(sum).head
      val sumBasicFactor = new BasicFactor[Double](sumSparseFactor.parents, sumSparseFactor.output)
      val arg1Indices = sumSparseFactor.parents(0).range.zipWithIndex
      val arg2Indices = sumSparseFactor.parents(1).range.zipWithIndex
      val resultIndices = sumv.range.zipWithIndex
      for {
        (arg1Val, arg1Index) <- arg1Indices
        (arg2Val, arg2Index) <- arg2Indices
        (resultVal, resultIndex) <- resultIndices
      } {
        sumBasicFactor.set(List(arg1Index, arg2Index, resultIndex), 0.0)
      }
      sumSparseFactor.getIndices.foreach(f => sumBasicFactor.set(f, sumSparseFactor.get(f)))

      val sparseTime = measureTime(() => sumSparseFactor.sumOver(p1v/*, SumProductSemiring*/), 3, 5)
      val denseTime = measureTime(() => sumBasicFactor.sumOver(p1v/*, SumProductSemiring*/), 3, 5)

      if (analyze) {

        println("Sparse factor size: " + sumSparseFactor.contents.size)
        println("Basic factor size: " + sumBasicFactor.contents.size)

        println("Performance ratio (dense/sparse) = " + denseTime / sparseTime)
      }

      denseTime / sparseTime should be >= 5.0
    }
  }

}












