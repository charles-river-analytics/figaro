  /**
   * DoubleFactorTest.scala
   * DoubleFactor tests.
   *
   * Created By:	Kathryn Rodgers (rodgersk@uci.edu)
   * Creation Date: 16 May 2014
   *
   *
   */

  package com.cra.figaro.test.algorithm.factored

import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.factored.{Factor, Variable}
import com.cra.figaro.language.{Select, _}
import org.scalatest.{Matchers, WordSpec}


  class DoubleFactorTest extends WordSpec with Matchers {

    "Math on Factor[Double]" when {
      "multiplying using *" should {
      "multiply the values in each Factor" in {
        val facts = setUp // generate factors to play with
        val factA = facts._1._1 // get Factor over A,B
        val factB = facts._1._2 // get Factor over B,C

        val res = factA * factB


        res shouldBe a [Factor[Double]]
        res.variables should equal(facts._2) // Should have List(A,B,C)
        // Check Values
        res.get(List(0,0,0)) should be (.07 +- .0001)
        res.get(List(1,0,0)) should be (.28 +- .0001)
        res.get(List(0,1,0)) should be (.16 +- .0001)
        res.get(List(1,1,0)) should be (.4 +- .0001)
        res.get(List(0,2,0)) should be (.27 +- .0001)
        res.get(List(1,2,0)) should be (.54 +- .0001)



      }
      }
      "adding using +" should {
        "add the values in each Factor" in {
    val facts = setUp // generate factors to play with
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B
    val factB:Factor[Double] = facts._1._2 // get Factor over B,C

    val res = factA ++ factB

    res shouldBe a [Factor[Double]]
    res.variables should equal(facts._2) // Should have List(A,B,C)
    // Check Values
    res.get(List(0,0,0)) should be (.8 +- .0001)
    res.get(List(1,0,0)) should be (1.1 +- .0001)
    res.get(List(0,1,0)) should be (1.0 +- .0001)
    res.get(List(1,1,0)) should be (1.3 +- .0001)
    res.get(List(0,2,0)) should be (1.2 +- .0001)
    res.get(List(1,2,0)) should be (1.5 +- .0001)


        }
      }
      "subtracting using - " should {
        "subtract the values of the factors" in {

    val facts = setUp // generate factors to play with
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B
    val factB:Factor[Double] = facts._1._2 // get Factor over B,C


    val res = factB-factA // so we don't get negative numbers

    res shouldBe a [Factor[Double]]
    res.variables should contain theSameElementsAs facts._2 // Should have List(A,B,C)
    // Check Values
    res.get(List(0,0,0)) should be (.6 +- .0001)
    res.get(List(1,0,0)) should be (.6 +- .0001)
    res.get(List(2,0,0)) should be (.6 +- .0001)
    res.get(List(0,0,1)) should be (.3 +- .0001)
    res.get(List(1,0,1)) should be (.3 +- .0001)
    res.get(List(2,0,1)) should be (.3 +- .0001)

        }
      }
      "dividing using /" should {
        "divide the double values of the factors" in {
    val facts = setUp // generate factors to play with
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B
    val factB:Factor[Double] = facts._1._2 // get Factor over B,C

    val res = factA / factB

    res shouldBe a [Factor[Double]]
    res.variables should equal(facts._2) // Should have List(A,B,C)
    // Check Values
    res.get(List(0,0,0)) - .142 should be <  .001
    res.get(List(1,0,0)) - .571 should be < .001
    res.get(List(0,1,0)) - .25 should be  < .01
    res.get(List(1,1,0)) - .625 should be < .001
    res.get(List(0,2,0)) - .333 should be < .001
    res.get(List(1,2,0)) -.666 should be <  .001

        }
      }
        "dividing with zeros and nans and infs using /" should{
    "result in zero always" in {
      val elemA = Select(.3->'z, .3 -> 'n, .3 -> 'i)
      val elemB = Select(.3->'z, .3 -> 'n, .3 -> 'i)
      val elemC = Select(.3->'z, .3 -> 'n, .3 -> 'i)

      Values()(elemA)
      Values()(elemB)
      Values()(elemC)

      val factA = new Factor[Double](List(Variable(elemA), Variable(elemB)))
      val factB = new Factor[Double](List(Variable(elemA), Variable(elemB)))


      factA.set(List(0,0), 0.0)
      factA.set(List(0,1), 0)
      factA.set(List(0,2), 0)
      factA.set(List(1,0), Double.NaN)
      factA.set(List(1,1), Double.NaN)
      factA.set(List(1,2), Double.NaN)
      factA.set(List(2,0), Double.PositiveInfinity)
      factA.set(List(2,1), Double.PositiveInfinity)
      factA.set(List(2,2), Double.PositiveInfinity)


      factB.set(List(0,0), 0.0)
      factB.set(List(0,1), Double.NaN)
      factB.set(List(0,2), Double.PositiveInfinity)
      factB.set(List(1,0), 0)
      factB.set(List(1,1), Double.NaN)
      factB.set(List(1,2), Double.PositiveInfinity)
      factB.set(List(2,0), 0)
      factB.set(List(2,1), Double.NaN)
      factB.set(List(2,2), Double.PositiveInfinity)

      val res = factA / factB

      res shouldBe a [Factor[Double]]

      res.get(List(0,0)) should be (0)
      res.get(List(0,1)) should be (0)
      res.get(List(0,2)) should be (0)
      res.get(List(1,0)) should be (0)
      res.get(List(1,1)) should be (0)
      res.get(List(1,2)) should be (0)
      res.get(List(2,0)) should be (0)
      res.get(List(2,1)) should be (0)
      res.get(List(2,2)) should be (0)

    }
        }
      "dividing by zero using normal division()" should {
        "result in NaN" in {

          val facts = setUp // generate factors to play with
          val factA:Factor[Double] = facts._1._1 // get Factor over A,B
          val factB:Factor[Double] = facts._1._2 // get Factor over B,C


          factB.set(List(0,0), 0)


          val res = factA.divide(factB)
          res shouldBe a [Factor[Double]]
          java.lang.Double.isNaN(res.get(List(0,0,0))) should be (true)
          res.get(List(0,1,0)) should be ((.2/.8) +- .001)
          res.get(List(0,2,0)) should be ((.3/.9) +- .001)
          java.lang.Double.isNaN(res.get(List(1,0,0))) should be (true)
          res.get(List(1,1,0)) should be ((.5/.8) +- .001)
          res.get(List(1,2,0)) should be ((.6/.9) +- .001)


        }
        "produce the division of double numbers" in {
          // Check Values
          val facts = setUp // generate factors to play with
          val factA:Factor[Double] = facts._1._1 // get Factor over A,B
          val factB:Factor[Double] = facts._1._2 // get Factor over B,C

          val res = factA.divide(factB)

    res shouldBe a [Factor[Double]]
    res.variables should equal(facts._2) // Should have List(A,B,C)
    (res.get(List(0,0,0)) - .142) should be < .001
    (res.get(List(1,0,0)) - .571) should be  < .001
    res.get(List(0,1,0)) - .25 should be < .001
    res.get(List(1,1,0)) - .625 should be < .001
    res.get(List(0,2,0)) - .333  should be < .001
    res.get(List(1,2,0)) - .666 should be < .001

        }
      }
      "computing e^(factor_values)" should {
        "give correct exp^() values" in {
    val facts = setUp // generate factors to play with ( (factors), (variables))
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B
    val factB:Factor[Double] = facts._1._2 // get Factor over B,C

    val resA = factA.exp
    val resB = factB.exp
    val resC = (factA ++ factB).exp // test evaluation of multiple functions


    resA shouldBe a [Factor[Double]]
    resB shouldBe a [Factor[Double]]
    resC shouldBe a [Factor[Double]]
    resA.variables should equal(factA.variables)
    resB.variables should equal(factB.variables)
    resC.variables should equal(facts._2)

    // Check Values
    // exp(factor A+B)
    resA.get(List(0,0)) should be (scala.math.exp(.1) +- .0001)
    resA.get(List(0,1)) should be (scala.math.exp(.2) +- .0001)
    resA.get(List(0,2)) should be (scala.math.exp(.3) +- .0001)
    resA.get(List(1,0)) should be (scala.math.exp(.4) +- .0001)
    resA.get(List(1,1)) should be (scala.math.exp(.5) +- .0001)
    resA.get(List(1,2)) should be (scala.math.exp(.6) +- .0001)

    // exp(factor B)
    resB.get(List(0,0)) should be (scala.math.exp(.7) +- .0001)
    resB.get(List(1,0)) should be (scala.math.exp(.8) +- .0001)
    resB.get(List(2,0)) should be (scala.math.exp(.9) +- .0001)

    // exp(factor A)
    resC.get(List(0,0,0)) should be (scala.math.exp(.8)  +- .0001)
    resC.get(List(1,0,0)) should be (scala.math.exp(1.1) +- .0001)
    resC.get(List(0,1,0)) should be (scala.math.exp(1.0) +- .0001)
    resC.get(List(1,1,0)) should be (scala.math.exp(1.3) +- .0001)
    resC.get(List(0,2,0)) should be (scala.math.exp(1.2) +- .0001)
    resC.get(List(1,2,0)) should be (scala.math.exp(1.5) +- .0001)
        }
      }
      "computing the log of a factor" should{
        "compute log of numbers > 0" in {
    val facts = setUp // generate factors to play with ( (factors), (variables))
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B

    val resA = factA.log

    resA shouldBe a [Factor[Double]]
    resA.variables should equal(factA.variables)
    // Check Values

    resA.get(List(0,0)) should be (scala.math.log(.1) +- .0001)
    resA.get(List(0,1)) should be (scala.math.log(.2) +- .0001)
    resA.get(List(0,2)) should be (scala.math.log(.3) +- .0001)
    resA.get(List(1,0)) should be (scala.math.log(.4) +- .0001)
    resA.get(List(1,1)) should be (scala.math.log(.5) +- .0001)
    resA.get(List(1,2)) should be (scala.math.log(.6) +- .0001)


        }
        "raising a factor to a power" should {
          "give the right answers" in {
            val facts = setUp
            val factA = facts._1._1
            val factB = facts._1._2

            val squared = factA ^ 2
            for (i<- squared.allIndices)
            {
              squared.get(i) should be (factA.get(i)*factA.get(i))
            }
            val rooted = factB ^ .5
            for(i<- rooted.allIndices)
            {
              rooted.get(i) should be (math.sqrt(factB.get(i)))
            }
          }
        }

      }


      "max of a DoubleFactor"  should {
        "return the maximum value of the factor" in {
    val facts = setUp // generate factors to play with
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B
    val factB:Factor[Double] = facts._1._2 // get Factor over B,C

    factB.set(List(2,0), .7)
    factB.set(List(0,0), .9) // change the order so the both max's aren't at last index in the factor

    val resA = factA.max((x,y) => x > y)
    val resB = factB.max((x,y) => x > y)

    //resA shouldBe a [Double]
    //resB shouldBe a [Double]

    resA should equal (.6)
    resB should equal (.9)


        }
      }
      "argMax of a DoubleFactor"  should {
        "return the values of the variables that maximize the factor's value" in {
    val facts = setUp // generate factors to play with
    val factA:Factor[Double] = facts._1._1 // get Factor over A,B
    val factB:Factor[Double] = facts._1._2 // get Factor over B,C

    factB.set(List(2,0), .7)
    factB.set(List(0,0), .9) // change the order so the both max's aren't at last index in the factor

    val resA = factA.argMax()
    val resB = factB.argMax()

    resA shouldBe a [List[Pair[Variable[_],Any]]]
    resB shouldBe a [List[Pair[Variable[_],Any]]]

    resA should not be (null)
    val expectedA = factA.variables.zip(List(false, 'middle))
    resA should contain theSameElementsInOrderAs(expectedA)

    resB should not be (null)
    val expectedB = factB.variables.zip(List('left,3))
    resB should contain theSameElementsInOrderAs(expectedB)

        }
      }


    }// end when

    /**
     * Returns ((factorAB, factorBC), List(variable(a), variable(b), variable(c)))
     *
     * So that set up code can be shared
     */
    def setUp:Tuple2[Tuple2[Factor[Double],Factor[Double]], List[Variable[_]]] = {
      val A = Flip(.3)
      val B = Select(.3->'left , .6->'right , .1->'middle)
      val C = Constant(3)
      Values()(A)
      Values()(B)
      Values()(C)

      val varis = List(Variable(A), Variable(B), Variable(C))
      val factA =  new Factor[Double](varis.slice(0, 2)) // Factor over A, B
      val factB = new Factor[Double](varis.slice(1,3)) // Factor over B, C

      // Set Factor's values
      factA.set(List(0,0), .1)
      factA.set(List(0,1), .2)
      factA.set(List(0,2), .3)
      factA.set(List(1,0), .4)
      factA.set(List(1,1), .5)
      factA.set(List(1,2), .6)

      factB.set(List(0,0), .7)
      factB.set(List(1,0), .8)
      factB.set(List(2,0), .9)



      ((factA,factB), List(varis(0), varis(1), varis(2)))



    }
  }

