/*
 * VERecursionTest.scala
 * Variable elimination test of tail recursion .
 *
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Jul 7, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.test.algorithm.factored

import org.scalatest.Matchers
import org.scalatest.{ WordSpec, PrivateMethodTester }
import scala.util.Random
import com.cra.figaro.language.{ Apply, Element, Flip, Universe }
import com.cra.figaro.library.atomic.continuous.{Uniform}
import com.cra.figaro.library.compound.{ If }
import com.cra.figaro.algorithm.factored.VariableElimination

/**
 * @author Glenn Takata (gtakata@cra.com)
 *
 */
class VERecursionTest extends WordSpec with Matchers {
  "Running VariableElimination" should {
    "with a very wide model produce the correct result" in {
      Universe.createNew()
      var root = Flip(0.5)

      val rand = new Random(System.currentTimeMillis)
      for (_ <- 0 until 10000) {
        val v = If(root, Flip(0.5), Flip(0.5))
        if ( rand.nextBoolean) {
          v.observe(true)
        }
        else {
          v.observe(false)
        }
      }
      test(root, (r: Boolean) => r == true, 0.50)
    }
  }

  def test[T](target: Element[T], predicate: T => Boolean, prob: Double) {
    val tolerance = 0.00001
    val algorithm = VariableElimination(target)
    algorithm.start()
    
    val dist = algorithm.distribution(target).toList
    println("\nTarget distribution: " + dist.mkString(",") + "\n")
    
    algorithm.probability(target, predicate) should be(prob +- tolerance)
    algorithm.kill()
  }
}