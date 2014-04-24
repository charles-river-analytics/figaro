package com.cra.appril.birdmigration.test

import scala.collection._
import scala.math._
import scala.math.exp
import scala.math.pow
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Apply
import com.cra.figaro.language.Constant
import com.cra.figaro.language.ElementCollection
import com.cra.figaro.language.Flip
import com.cra.figaro.language.NonCachingChain
import com.cra.figaro.language.Select
import com.cra.figaro.library.atomic._
import com.cra.figaro.library.atomic.discrete.Binomial
import JSci.maths.ExtraMath.factorial
import com.cra.figaro.language.Name
import com.cra.figaro.language.CachingChain
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.language.Chain
import com.cra.appril.birdmigration.elements.Counter

object ConstraintTest {

  val factorialSizeLimit = 14

  def logFactorial(n: Int): Double = {

    if (n < 0) {
      throw new IllegalArgumentException
    }
    if (n == 0 || n == 1) {
      0.0;
    }
    if (n <= factorialSizeLimit) {
      var z: Long = 1;
      var x: Long = 1;
      for (i <- 2 to n) {
        x += 1;
        z *= x;
      }
      return Math.log(z);
    } else {
      var x = (n + 1).toDouble;
      var y = 1.0 / (x * x);
      var z = ((-(5.95238095238E-4 * y) + 7.936500793651E-4) * y -
        2.7777777777778E-3) * y + 8.3333333333333E-2;
      z = ((x - 0.5) * Math.log(x) - x) + 9.1893853320467E-1 + z / x;
      return z;
    }
  }

  //def density(d: Double) = {
  //    val diff = d - mean
  //val exponent = -(diff * diff) / (2.0 * variance)
  //normalizer * exp(exponent)
  //  }

  def normalDensity(d: Double, mean: Double, stdDev: Double) = {
    val variance = stdDev * stdDev
    val normalizer = 1.0 / sqrt(2.0 * Pi * variance)
    val diff = d - mean
    val exponent = -(diff * diff) / (2.0 * variance)
    normalizer * exp(exponent)
  }

  //Normpdf(n.toDouble, birdsObservedInCell.toDouble, stddev)

  /*
  def poissonDensity(lambda: Double, k: Int) = {
    val expMinusLambda = exp(-lambda)
    val result = pow(lambda, k) / factorial(k) * expMinusLambda
    result
  }
  */
  //A lot faster than the original
  def poissonDensity(lambda: Double, k: Int) = {
    val lambdaToK = pow(lambda, k)
    val logLambdaToK = Math.log(lambdaToK)
    val logFactorialK = logFactorial(k)
    val logResult = (logLambdaToK - logFactorialK - lambda)
    val result = Math.exp(logResult)
    result
  }
  
  def constraintTest2 = {

    val x = Select(0.95 -> 0, 0.05 -> 2)
    val noTrials = Constant(0)//Apply(x, (i: Int) => if (i > 0 ) i*5 else 0)

    val a1 = Select(0.25 -> 0.25, 0.50 -> 0.50, 0.75 -> 0.75)
    val b1 = Binomial(noTrials,a1)
    
    val t = List(noTrials,a1,b1)
    val inferenceAlgorithm = BeliefPropagation.lazyBP(10, 200, true, t: _*)
    //val inferenceAlgorithm = VariableElimination(t:_*)
    println("Begin.")
    inferenceAlgorithm.start
    println("finished.")
    println("Expectation:" + inferenceAlgorithm.expectation(a1, (d: Double) => d))


    inferenceAlgorithm.stop
    inferenceAlgorithm.kill
  }
  
/*
  def constraintTest1 = {

    val p1 = Constant(0.95)//discrete.Uniform(0.05, 0.95)
    //val p2 = discrete.Uniform(0.25,0.50,0.75)
    //val p3 = discrete.Uniform(0.25,0.50,0.75)
    //val p4 = discrete.Uniform(0.25,0.50,0.75)

    val x11 = 0.85
    //val x12 = 0.894427191
    //val x13 = -0.948683298
    //val x14 = 0

    val x21 = 0.05
    //val x22 = 1
    //val x23 = -0.707106781
    //val x24 = 0

    val x31 = 0.911466171
    //val x32 = 0.998274373	
    //val x33 = -0.707106781
    //val x34 = 0

    val x41 = 0.700171152
    //val x42 = 0.928476691	
    //val x43 = -0.447213595
    //val x44 = 0

    val start1 = Constant(4)
    val start2 = Constant(0)

    //Want to get expectation where beta = 0.95

    //prob of going to cell 1
    //Beta = 0.05   = 0.95
    //0.510560929	0.690562425
    val a1 = Constant(0.70)/*Apply(p1, (b1: Double) => {

      val numerator = math.exp(
        (b1 * x11))

      var denominator = 0.0

      denominator += math.exp(
        (b1 * x11))

      denominator += math.exp(
        (b1 * x21))

      val result = numerator / denominator
      println("THETA1:" + result)
      result
    })*/

    //prob of going to cell 2

    //Beta = 0.05   = 0.95
    //0.489439071	0.309437575
    val a2 = Constant(0.30)/*Apply(p1, (b1: Double) => {

      val numerator = math.exp(
        (b1 * x21))

      var denominator = 0.0

      denominator += math.exp(
        (b1 * x11))

      denominator += math.exp(
        (b1 * x21))

      val result = numerator / denominator
      println("THETA2:" + result)
      result
    })
    */
    
    
    

    //Number moving from cell 1 to cell 1
    val b1 = Chain(start1, (i: Int) => if (i > 0) Binomial(i, a1) else Constant(0))
    //Number moving from cell 1 to cell 2
    val b2 = Chain(start2, (i: Int) => if (i > 0) Binomial(i, a2) else Constant(0))
    val z1 = Counter(List(b1))
    val z2 = Counter(List(b2))

    //One of the birds moved
    val o1 = 2
    val o2 = 2
/*
    z1.setConstraint((n: Int) => {
      val result = normalDensity(n.toDouble, o1.toDouble, 0.15)
      println("set constraintz1 (c,n,o,r): " + " " + n + " " + o1 + " " + result)
      result
    })

    z2.setConstraint((n: Int) => {
      val result = normalDensity(n.toDouble, o2.toDouble, 0.15)
      println("set constraintz2 (c,n,o,r): " + " " + n + " " + o2 + " " + result)
      result
    })
*/
    /*
    //1 parameter, two cells.
    //Only condsider movement from cell 1 to cell 2.
    //Same conditions as previous time step
    val a3 = Constant(0.70)/*Apply(p1, (b1: Double) => {

      val numerator = math.exp(
        (b1 * x11))

      var denominator = 0.0

      denominator += math.exp(
        (b1 * x11))

      denominator += math.exp(
        (b1 * x21))

      val result = numerator / denominator
      println("THETA1:" + result)
      result
    })
*/
    //prob of going to cell 2

    //Beta = 0.05   = 0.95
    //0.489439071	0.309437575
    val a4 = Constant(0.30)/*Apply(p1, (b1: Double) => {

      val numerator = math.exp(
        (b1 * x21))

      var denominator = 0.0

      denominator += math.exp(
        (b1 * x11))

      denominator += math.exp(
        (b1 * x21))

      val result = numerator / denominator
      println("THETA2:" + result)
      result
    })
    * 
    */
    //Number moving from cell 1 to cell 1
    val b3 = Chain(z1, (i: Int) => if (i > 0) Binomial(i, a3) else Constant(0))
    //Number moving from cell 1 to cell 2
    val b4 = Chain(z1, (i: Int) => if (i > 0) Binomial(i, a4) else Constant(0))
    val z3 = Counter(List(b3))
    val z4 = Counter(List(b4))

    //one of birds moved
    val o3 = 1
    val o4 = 3

    z3.setConstraint((n: Int) => {
      val result = normalDensity(n.toDouble, o3.toDouble, 0.15)
      println("set constraintz1 (c,n,o,r): " + " " + n + " " + o1 + " " + result)
      result
    })

    z4.setConstraint((n: Int) => {
      val result = normalDensity(n.toDouble, o4.toDouble, 0.15)
      println("set constraintz2 (c,n,o,r): " + " " + n + " " + o2 + " " + result)
      result
    })
    */

        //1 parameter, two cells
    //Only condsider movement from cell 1 to cell 2.
    //Same conditions as previous time step
    val a5 = Constant(0.70)/*Apply(p1, (b1: Double) => {

      val numerator = math.exp(
        (b1 * x11))

      var denominator = 0.0

      denominator += math.exp(
        (b1 * x11))

      denominator += math.exp(
        (b1 * x21))

      val result = numerator / denominator
      println("THETA1:" + result)
      result
    })*/

    //prob of going to cell 2

    //Beta = 0.05   = 0.95
    //0.489439071	0.309437575
    val a6 = Constant(0.30)/*Apply(p1, (b1: Double) => {

      val numerator = math.exp(
        (b1 * x21))

      var denominator = 0.0

      denominator += math.exp(
        (b1 * x11))

      denominator += math.exp(
        (b1 * x21))

      val result = numerator / denominator
      println("THETA2:" + result)
      result
    })*/
    //Number moving from cell 1 to cell 1
    val b5 = Chain(z1, (i: Int) => if (i > 0) Binomial(i, a5) else Constant(0))
    //Number moving from cell 1 to cell 2
    val b6 = Chain(z1, (i: Int) => if (i > 0) Binomial(i, a6) else Constant(0))
    val z5 = Counter(List(b5))
    val z6 = Counter(List(b6))

    //one of birds moved
    val o5 = 0
    val o6 = 4
/*
    z5.setConstraint((n: Int) => {
      val result = normalDensity(n.toDouble, o5.toDouble, 0.15)
      result
    })

    z6.setConstraint((n: Int) => {
      val result = normalDensity(n.toDouble, o6.toDouble, 0.15)
      result
    })
 */ 
    
   
    
    //val t = List(p1,a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,z1,z2,z3,z4,z5,z6)
    val t = List(p1,a1,a2,a5,a6,b1,b2,b5,b6,z1,z2,z5,z6)
    //val inferenceAlgorithm = BeliefPropagation.lazyBP(10, 2, true, t: _*)
    //MH gives 0.890626
    //val inferenceAlgorithm = MetropolisHastings(100000,ProposalScheme.default,t:_*)
    val inferenceAlgorithm = VariableElimination(t:_*)
    println("Begin.")
    inferenceAlgorithm.start

    println("Expectation:" + inferenceAlgorithm.expectation(p1, (d: Double) => d))
    //val pv1 = inferenceAlgorithm.probabilityBounds(t.head, 0.05)
    //val pv2 = inferenceAlgorithm.probabilityBounds(t.head, 0.95)
    //val pv1Lower = (pv1._1 * 0.05 + pv1._1 * 0.95) / 2.0
    //val pv1Upper = (pv1._2 * 0.05 + pv1._2 * 0.95) / 2.0
    //println("Estimated value of p1(lower bound): " + pv1Lower)
    //println("Estimated value of p1(upper bound): " + pv1Upper)

    inferenceAlgorithm.stop
    inferenceAlgorithm.kill
  }
*/
  /*
  def binomialTest = {
    val trials = 7

    //Changing binomial to caching fixes.
    //Implies problem with noncaching chains.
    val p = Select(0.25 -> 0.25, 0.75 -> 0.75)
    val c = ElementCollection.default
    val n : Name[Boolean] = "name"
    val e = new NonCachingChain[Double, Boolean](n, p, (p: Double) => Flip(p), c)
    //val inferenceAlgorithm = VariableElimination.debugged(e,p)
    
    //val p = Select(0.25 -> 0.25, 0.75 -> 0.75)
    //val b = Binomial(trials, p)
    //b.observe(6)
    val t = List(e,p)
    
    val inferenceAlgorithm = BeliefPropagation.lazyBP(10,100, true, t:_*)
    //val inferenceAlgorithm = BeliefPropagation(t:_*)
    //val inferenceAlgorithm = MetropolisHastings(100000,ProposalScheme.default,t:_*)
    inferenceAlgorithm.start
    
    
    println("Expectation:" + inferenceAlgorithm.expectation(p, (d: Double) => d))
    val pv1 = inferenceAlgorithm.probabilityBounds(t.head, 0.05)
    val pv2 = inferenceAlgorithm.probabilityBounds(t.head, 0.95)
    val pv1Lower = (pv1._1*0.05 + pv1._1*0.95)/2.0
    val pv1Upper = (pv1._2*0.05 + pv1._2*0.95)/2.0
    
    println("Estimated value of b1(lower bound): " + pv1Lower)
    println("Estimated value of b1(upper bound): " + pv1Upper)
    inferenceAlgorithm.stop
    inferenceAlgorithm.kill

  }
  */
  def main(args: Array[String]): Unit = {

    constraintTest2
  }
}


		  