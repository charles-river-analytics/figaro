package com.cra.appril.birdmigration.model
import com.cra.figaro.language.Element
import scala.math.{ exp, pow, ceil, log }
import JSci.maths.ExtraMath.factorial
import com.cra.figaro.language.Apply
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.Abstraction
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.appril.birdmigration.elements.Multinomial
import com.cra.appril.birdmigration.elements.Counter
import com.cra.figaro.language.Chain
import scala.collection.mutable.ListBuffer
import com.cra.figaro.language.ElementCollection
import com.cra.figaro.language.Element
import com.cra.figaro.language.Apply
import com.cra.appril.birdmigration.elements.Counter
import com.cra.figaro.library.atomic.discrete.Binomial
import scala.collection._
import com.cra.figaro.language.Constant
import scala.math.{ exp, pow }
import com.cra.figaro.library.atomic.discrete.Poisson
import com.cra.figaro.language.Inject
import com.cra.appril.birdmigration.elements.CounterAbstraction
import com.cra.figaro.algorithm.Abstraction
import JSci.maths.ExtraMath.factorial
import scala.math._
import com.cra.figaro.library.atomic._
import com.cra.figaro.algorithm.{ AbstractionScheme, Abstraction }
import com.cra.appril.birdmigration.elements.Multinomial
import com.cra.figaro.language.CachingChain
import com.cra.figaro.language.NonCachingChain

//To be used for estimating parameters.
class ParameterModel {

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

  //A lot faster than the original
  def poissonDensity(lambda: Double, k: Int) = {
    val lambdaToK = pow(lambda, k)
    val logLambdaToK = Math.log(lambdaToK)
    val logFactorialK = logFactorial(k)
    val logResult = (logLambdaToK - logFactorialK - lambda)
    val result = Math.exp(logResult)
    result
  }

  val allTransitions = Array.empty[Element[Int]]
  //Create a multinomial for every nonzero cell at every timestep
  //Set constraint on result.

  def makeTheta(fromCell: Int, toCell: Int, data: ModelData, year: Int, night: Int): Element[Double] = {
    //println("made theta: " + fromCell + " " + toCell + " " + night)
    if (data.isReachable(fromCell, toCell) == false) {
      println("unreachable.")
      val theta = Constant(0.0)
      theta
    } else {

      //println("Creating real theta.")
      //Needs to be normalized by reachable cells.
      //Just have to know all conditions instead of relevant. Not a big deal
      val x1 = data.getDistanceBetweenCells(year, night, fromCell, toCell)
      val x2 = data.getVectorDifferenceBetweenCells(year, night, fromCell, toCell)
      val x3 = data.getWindDirectionBetweenCells(year, night, fromCell, toCell)
      val x4 = data.getSameCellBetweenCells(year, night, fromCell, toCell)

      val cellsReachable = data.getReachableFrom(fromCell)
      //If this is a hindrance to performance, these lists can be precomputed in the ModelData class instead
      //of redoing them over and over in here.

      //This represents all the other phis which we must use as normalization
      val allX1 = cellsReachable.map((to: Int) => data.getDistanceBetweenCells(year, night, fromCell, to))
      val allX2 = cellsReachable.map((to: Int) => data.getVectorDifferenceBetweenCells(year, night, fromCell, to))
      val allX3 = cellsReachable.map((to: Int) => data.getWindDirectionBetweenCells(year, night, fromCell, to))
      val allX4 = cellsReachable.map((to: Int) => data.getSameCellBetweenCells(year, night, fromCell, to))

      val numberReachable = cellsReachable.size
      val theta = Apply(data.beta1, data.beta2, data.beta3, data.beta4, (b1: Double, b2: Double, b3: Double, b4: Double) => {

        val numerator = math.exp(
          (b1 * x1) +
            (b2 * x2) +
            (b3 * x3) +
            (b4 * x4))

        var denominator = 0.0
        for (i <- 0 to numberReachable - 1) {
          denominator += math.exp(
            (b1 * allX1(i)) +
              (b2 * allX2(i)) +
              (b3 * allX3(i)) +
              (b4 * allX4(i)))
        }
        val result = if (denominator == 0) {
          println("denominator is zero.")
          0
        } else {
          numerator / denominator
        }

        //println(result)
        result
      })
      theta
    }
  }

  def doModel(data: ModelData) = {

    for (year <- data.years) {

      for (day <- data.days) {
        val allCellsReachable = mutable.Set.empty[Int]
        if (day < 20) {
          println("day: " + day)
          val birdsTransitioningToCell = mutable.Map.empty[Int, ListBuffer[Element[Int]]]
          for (cell <- data.cells) {
            if (data.getBirdsObservedInCell(year, day, cell) > 0) {
              val cellsReachable = data.getReachableFrom(cell)
              allCellsReachable ++= cellsReachable
              val N = data.getBirdsObservedInCell(year, day, cell)
              val probs: List[Element[Double]] = for (toCell <- cellsReachable)
                yield makeTheta(cell, toCell, data, year, day)//Theta is computed using all reachable cells.

			    val result = mutable.Map.empty[Int, Element[Int]]
			
			    for ((prob, outcome) <- probs zip cellsReachable) {
			      if (data.getBirdsObservedInCell(year, day+1, outcome) > 0) {//But we only make a binomial if the destination actually has birds in it
			    	  val e = Binomial(N, prob)
	    			  result += outcome -> e
			      }
			    }
			    val m = result.toMap

              for (toCell <- cellsReachable) {

                if (m.contains(toCell) && birdsTransitioningToCell.contains(toCell)) {
                  birdsTransitioningToCell(toCell) += m(toCell)
                } else if (birdsTransitioningToCell.contains(toCell)) {
                  birdsTransitioningToCell += toCell -> ListBuffer[Element[Int]](m(toCell))
                }
              }
            } //End reachable

          } //End cell

          //If this could have been reached from a cell with a bird in it.
          for (cell <- allCellsReachable) {
            //And if the cell actually does have a bird in it
            if (day < 20 && data.getBirdsObservedInCell(year, day + 1, cell) > 0) {
              val numberOfBirdsInCurrentCell: Element[Int] = if (birdsTransitioningToCell.contains(cell)) {
                Counter(birdsTransitioningToCell(cell).toList, data.numberOfBirds)
              } else {
                Constant(0)
              }
              val birdsObservedInCell = data.getBirdsObservedInCell(year, day + 1, cell)

              numberOfBirdsInCurrentCell.setConstraint((n: Int) => {
                val result = poissonDensity(n, birdsObservedInCell)
                result
              })
              //numberOfBirdsInCurrentCell.addPragma(Abstraction(5)(CounterAbstraction))
            }
          }
          birdsTransitioningToCell.clear
        }
        allCellsReachable.clear
      }

    }

    val t = List(data.beta1, data.beta2, data.beta3, data.beta4)
    println("Start.")
    val inferenceAlgorithm = MetropolisHastings(200000, ProposalScheme.default, t: _*)
    inferenceAlgorithm.start()
    println("Done.")
    val b1 = inferenceAlgorithm.expectation(data.beta1, (d: Double) => d)
    val b2 = inferenceAlgorithm.expectation(data.beta2, (d: Double) => d)
    val b3 = inferenceAlgorithm.expectation(data.beta3, (d: Double) => d)
    val b4 = inferenceAlgorithm.expectation(data.beta4, (d: Double) => d)
      
    println(b1)
    println(b2)
    println(b3)
    println(b4)
    
    inferenceAlgorithm.stop
    inferenceAlgorithm.kill
  }

}