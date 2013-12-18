
/* * SemiringTest.scala 
 * Semiring tests.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.factored

import org.scalatest.{ WordSpec, PrivateMethodTester }
import org.scalatest.matchers.ShouldMatchers
import com.cra.figaro._
import com.cra.figaro.util._
import language._
import com.cra.figaro.algorithm.factored.SumProductSemiring
import com.cra.figaro.algorithm.factored.MaxProductSemiring
import com.cra.figaro.algorithm.factored.SumProductUtilitySemiring
import com.cra.figaro.algorithm.factored.SufficientStatisticsSemiring
import com.cra.figaro.algorithm.factored.Semiring
import scala.collection._

import com.cra.figaro.library.atomic.continuous.Uniform
import algorithm._
import factored._
import library.compound.If
import sampling.ProbEvidenceSampler
import com.cra.figaro.library.atomic.continuous._
class SemiringTest extends WordSpec with ShouldMatchers with PrivateMethodTester {

  "The joint semiring" should
    {
      "correctly multiply two numbers" in
        {
          val semiring = SumProductUtilitySemiring
          semiring.product((0.5, 0.5), (0.5, 0.5)) should equal((0.25, 1))
        }

      "correctly add two numbers" in
        {
          val semiring = SumProductUtilitySemiring
          semiring.sum((0.5, 0.5), (0.5, 0.5)) should equal((1, 0.5))
          val s2 = semiring.sum((1, 1), (2, 2))
          s2._1 should be(3.0 plusOrMinus 0.000000001)
          s2._2 should be(5.0 / 3.0 plusOrMinus 0.0000000001)
          val s3 = semiring.sum((0.5, 0.5), (1, 1))
          s3._1 should be(1.5 plusOrMinus 0.000000001)
          s3._2 should be(5.0 / 6.0 plusOrMinus 0.0000000001)
        }

      "satisfy semiring properties for random data" in
        {
          val semiring = SumProductUtilitySemiring

          //Tolerance could be hard coded inside function.
          def probPlusOrMinus(x: (Double, Double), y: (Double, Double), epsilon: (Double, Double)): Boolean =
            {
              if (x._1 + epsilon._1 > y._1 && x._2 - epsilon._2 < y._2) true else false
            }

          for (j <- 1 to 100000) {
            semiringProperties[(Double, Double)](semiring, (random.nextDouble(), random.nextDouble()), (random.nextDouble(), random.nextDouble()), (random.nextDouble(), random.nextDouble()), probPlusOrMinus, (0.001, 0.001))
          }
        }
    }


  "The max-product semiring" should
    {
      "correctly multiply two numbers" in
        {
          val semiring = MaxProductSemiring
          semiring.product(0.5, 0.5) should equal(0.25)
        }

      "correctly add two numbers" in
        {
          val semiring = MaxProductSemiring
          semiring.sum(0.5, 0.5) should equal(0.5)
          semiring.sum(1, 2) should equal(2)
          semiring.sum(0.5, 1) should equal(1)
        }

      "satisfy semiring properties for random data" in
        {
          val semiring = SumProductSemiring

          //Tolerance could be hard coded inside function.
          def probPlusOrMinus(x: Double, y: Double, epsilon: Double): Boolean =
            {
              if (x + epsilon > y && x - epsilon < y) true else false
            }

          for (j <- 1 to 100000) {
            semiringProperties(semiring, random.nextDouble(), random.nextDouble(), random.nextDouble(), probPlusOrMinus, 0.001)
          }
        }

    }

  "The sum-product semiring" should
    {
      "correctly multiply two numbers" in
        {
          val semiring = SumProductSemiring
          semiring.product(0.5, 0.5) should equal(0.25)
        }

      "correctly add two numbers" in
        {
          val semiring = SumProductSemiring
          semiring.sum(0.5, 0.5) should equal(1.0)
        }

      "satisfy semiring properties for random data" in
        {
          val semiring = SumProductSemiring

          //Tolerance could be hard coded inside function.
          def probPlusOrMinus(x: Double, y: Double, epsilon: Double): Boolean =
            {
              if (x + epsilon > y && x - epsilon < y) true else false
            }

          for (j <- 1 to 100000) {
            semiringProperties(semiring, random.nextDouble(), random.nextDouble(), random.nextDouble(), probPlusOrMinus, 0.001)
          }
        }

    }

  "The sufficient statistics semiring" should
    {

      "handle zero values in weighted multiplication without crashing" in
        {
          val numberOfParameters = 1
          val param = DirichletParameter(1,1,1)
          val parameterMap = mutable.Map.empty[Parameter[_], Seq[Double]]
          parameterMap += param -> Seq(0.0, 0.0, 0.0)

          val semiring = new SufficientStatisticsSemiring(parameterMap.toMap)

          val allZeros = semiring.sum((0.50, mutable.Map(param -> Seq(0.0, 0.0, 0.0))), (0.50, mutable.Map(param -> Seq(0.0, 0.0, 0.0))))

          allZeros._2(param)(0) should be(0.0 plusOrMinus 0.001)
          allZeros._2(param)(1) should be(0.0 plusOrMinus 0.001)
          allZeros._2(param)(2) should be(0.0 plusOrMinus 0.001)

          val leftZeros = semiring.sum((0.50, mutable.Map(param -> Seq(0.0, 0.0, 0.0))), (0.50, mutable.Map(param -> Seq(0.2, 0.6, 0.4))))

          leftZeros._2(param)(0) should be(0.1 plusOrMinus 0.001)
          leftZeros._2(param)(1) should be(0.3 plusOrMinus 0.001)
          leftZeros._2(param)(2) should be(0.2 plusOrMinus 0.001)

          val rightZeros = semiring.sum((0.50, mutable.Map(param -> Seq(0.3, 0.4, 0.5))), (0.50, mutable.Map(param -> Seq(0.0, 0.0, 0.0))))

          rightZeros._2(param)(0) should be(0.15 plusOrMinus 0.001)
          rightZeros._2(param)(1) should be(0.2 plusOrMinus 0.001)
          rightZeros._2(param)(2) should be(0.25 plusOrMinus 0.001)
        }

      "correctly multiply two numbers" in
        {
          val param = DirichletParameter(1,1,1)
          val parameterMap = mutable.Map.empty[Parameter[_], Seq[Double]]
          parameterMap += param -> Seq(0.0, 0.0, 0.0)


          val semiring = SufficientStatisticsSemiring(parameterMap.toMap)

          val result = semiring.product((0.50, mutable.Map(param -> Seq(0.3, 0.4, 0.5))), (0.50, mutable.Map(param -> Seq(0.8, 0.432, 0.0))))
          result._1 should equal(0.25)
          //The simple product is used in the multiplication step.
          result._2(param)(0) should be(1.1 plusOrMinus 0.001)
          result._2(param)(1) should be(0.832 plusOrMinus .001)
          result._2(param)(2) should be(0.5 plusOrMinus .001)

        }

      "correctly add two numbers" in
        {
          val param = DirichletParameter(1,1,1)
          val parameterMap = mutable.Map.empty[Parameter[_], Seq[Double]]
          parameterMap += param -> Seq(0.0, 0.0, 0.0)
      
          val semiring = SufficientStatisticsSemiring(parameterMap.toMap)

          val result = semiring.sum((0.50, mutable.Map(param -> Seq(0.3, 0.4, 0.5))), (0.50, mutable.Map(param -> Seq(0.8, 0.432, 0.0))))
          result._1 should be(1.0 plusOrMinus 0.001)
          //(.5*.3 + .5*.8)/1 = 0.15 + .4 = .55
          result._2(param)(0) should be(0.55 plusOrMinus 0.001)
          //.5*.4 + .5*.432/1 = .2 + .216 = .416
          result._2(param)(1) should be(.416 plusOrMinus 0.001)
          //.5*.5 + .5*0/1 = .25
          result._2(param)(2) should be(0.25 plusOrMinus 0.001)
        }

      "satisfy semiring properties for random data" in
        {
           val p1 = DirichletParameter(1)   
           val p2 = BetaParameter(1,1)
           val p3 = DirichletParameter(1,1,1)
           val p4 = DirichletParameter(1,1,1,1)
           val p5 = DirichletParameter(1,1,1,1,1)
           val p6 = DirichletParameter(1,1,1,1,1,1)
           val p7 = DirichletParameter(1,1,1,1,1,1,1)
           val p8 = DirichletParameter(1,1,1,1,1,1,1,1)
           val p9 = DirichletParameter(1,1,1,1,1,1,1,1,1)
           val p10 = DirichletParameter(1,1,1,1,1,1,1,1,1,1)

          val possibleParameterVectors = immutable.Map(
            p1 -> Seq(0.0),
            p2 -> Seq(0.0, 0.0),
            p3 -> Seq(0.0, 0.0, 0.0),
            p4 -> Seq(0.0, 0.0, 0.0, 0.0),
            p5 -> Seq(0.0, 0.0, 0.0, 0.0, 0.0),
            p6 -> Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
            p7 -> Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
            p8 -> Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
            p9 -> Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
            p10 -> Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
          for (j <- 1 to 100000) {

            //Decide vector lengths
            val numberOfParameters = random.nextInt(30) + 1
            val zeroSufficientStatisticsMap= mutable.Map.empty[Parameter[_], Seq[Double]]

            for (p <- 1 to numberOfParameters) {
              val r = random.nextInt(10)
              if (r == 0)
              {
                  val p = DirichletParameter(1)
                  val s = Seq(0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if(r == 1)
              {
                  val p = DirichletParameter(1,1)
                  val s = Seq(0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if(r == 2)
              {
                  val p = DirichletParameter(1,1,1)
                  val s = Seq(0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if(r == 3)
              {
                  val p = DirichletParameter(1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if(r == 4)
              {
                  val p = DirichletParameter(1,1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if(r == 5)
              {
                  val p = DirichletParameter(1,1,1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if (r == 6)
              {
                  val p = DirichletParameter(1,1,1,1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if (r == 7) 
              {
                  val p = DirichletParameter(1,1,1,1,1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else if( r == 8)
              {
                  val p = DirichletParameter(1,1,1,1,1,1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
              else
              {
                  val p = DirichletParameter(1,1,1,1,1,1,1,1,1,1)
                  val s = Seq(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
            	  zeroSufficientStatisticsMap += p -> s
              }
        
              
              
            }


            def randomParameterMap(): mutable.Map[Parameter[_], Seq[Double]] =
              {

                val paramMap = mutable.Map.empty[Parameter[_], Seq[Double]]
                for (p <- zeroSufficientStatisticsMap.keys) {
                  paramMap += p -> zeroSufficientStatisticsMap(p)
                  for (index <- 0 to paramMap(p).size - 1) {
                    (paramMap(p))(index).+(random.nextDouble())
                  }
                }
                paramMap
              }

            def create(): (Double, Map[Parameter[_], Seq[Double]]) =
              {
                val paramMap = randomParameterMap()
                val prob = random.nextDouble()
                (prob, paramMap)
              }

            val a = create
            val b = create
            val c = create

            def probPlusOrMinus(x: (Double, Map[Parameter[_], Seq[Double]]), y: (Double, Map[Parameter[_], Seq[Double]]), epsilon: (Double, Map[Parameter[_], Seq[Double]])): Boolean =
              {
                var result1 = true
                var result2 = true
                var result3 = true

                //Double
                if (x._1 + epsilon._1 < y._1 || x._1 - epsilon._1 > y._1) {
                  result1 = false
                }

                //Vectors of double
                for (key <- x._2.keys) {
                  val xVector = x._2(key)
                  val yVector = y._2(key)

                  for ((x1, y1) <- xVector zip yVector) {
                    if (x1 + epsilon._1 < y1 || x1 - epsilon._1 > y1) {
                      result2 = false
                    }
                  }

                }
                //I don't think there can be round-off errors with boolean.
                result1 && result2
              }
    
            val semiring = new SufficientStatisticsSemiring(zeroSufficientStatisticsMap.toMap)
            semiringProperties[(Double, Map[Parameter[_], Seq[Double]])](semiring.asInstanceOf[com.cra.figaro.algorithm.factored.Semiring[(Double, Map[Parameter[_], Seq[Double]])]], a, b, c, probPlusOrMinus, (0.001, zeroSufficientStatisticsMap.toMap))
          }
        }
    }

  //Because the structure may be more complicated than an operation on two primitive types, we
  //can't use the built-in 'plusOrMinus' from the ScalaTest package. Instead, define an appropriate
  //+/- function and include it when this method is called.
  def semiringProperties[T1](semiring: Semiring[T1], a: T1, b: T1, c: T1, withPlusOrMinus: (T1, T1, T1) => Boolean, tolerance: T1) {
    val one = semiring.one
    val zero = semiring.zero

    //(a + b) + c = a + (b + c)
    withPlusOrMinus(semiring.sum(semiring.sum(a, b), c), semiring.sum(a, semiring.sum(b, c)), tolerance) should equal(true)
    //0 + a = a + 0
    withPlusOrMinus(semiring.sum(zero, a), semiring.sum(a, zero), tolerance) should equal(true)
    //0 + a = a
    withPlusOrMinus(semiring.sum(zero, a), a, tolerance) should equal(true)
    //(a + b) = (b + a)
    withPlusOrMinus(semiring.sum(a, b), semiring.sum(b, a), tolerance) should equal(true)
    //(a * b) * c = a * (b * c)
    withPlusOrMinus(semiring.product(semiring.product(a, b), c), semiring.product(a, semiring.product(b, c)), tolerance) should equal(true)
    //1*a = a*1
    withPlusOrMinus(semiring.product(one, a), semiring.product(a, one), tolerance) should equal(true)
    //1*a = a
    withPlusOrMinus(semiring.product(one, a), a, tolerance) should equal(true)
    //a * (b + c) = (a * b) + (a * c)
    withPlusOrMinus(semiring.product(a, semiring.sum(b, c)), semiring.sum(semiring.product(a, b), semiring.product(a, c)), tolerance) should equal(true)
    //(a + b) * c = (a * c) + (b * c)
    withPlusOrMinus(semiring.product(semiring.sum(a, b), c), semiring.sum(semiring.product(a, c), semiring.product(b, c)), tolerance) should equal(true)
  }

}
