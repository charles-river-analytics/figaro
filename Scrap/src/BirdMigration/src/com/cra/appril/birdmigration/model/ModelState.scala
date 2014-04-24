package com.cra.appril.birdmigration.model

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
abstract class ModelState(val data: ModelData, val year: Int, val day: Int) extends ElementCollection {

  val numberOfBirdsInCell: Map[Int, Element[Int]]
  val night = day - 1
  
    val nextState: Element[ModelState] = 
      if (day == data.endDay) {
     
        Constant(this)
      } else {
        
        Apply(Constant(this), (s: ModelState) => ModelState(s, s.data, s.year, s.day + 1))
      }
      
      val lastCell = data.cells.last
    lazy val birdReachedDestination: Element[Boolean] =
      if (day == data.endDay) Apply(numberOfBirdsInCell(lastCell), 
          {
          (i: Int) => i > 0})
      else CachingChain(nextState, (s: ModelState) => { 
        Apply(s.birdReachedDestination,numberOfBirdsInCell(lastCell), (b: Boolean,i: Int) => b || i > 0)
        })

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

  //Is there any reason not to make all of these up front?
  //
  def makeTheta(fromCell: Int, toCell: Int): Element[Double] = {
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
        }
        else
        {
          numerator / denominator
        }
        
        //println(result)
        result
      })
      theta.addPragma(Abstraction(5))
      theta
    }
  }

}

class InitialState(year: Int, day: Int)(data: ModelData) extends ModelState(data, year, day) {

  val numberOfBirdsInCell = mutable.Map.empty[Int, Element[Int]]
  for (cell <- data.cells) {
    if (cell == 1) {
      //All birds begin in lower left.
      numberOfBirdsInCell += cell -> Constant(data.numberOfBirds)
    }
    else {
      numberOfBirdsInCell += cell -> Constant(0)
    }
      
  }

}

class IntermediateState(val previousState: ModelState, year: Int, day: Int)(data: ModelData) extends ModelState(data, year, day) {

  //Create a transition from every cell to every (reachable) cell 
  val birdsTransitioningToCell = mutable.Map.empty[Int, ListBuffer[Element[Int]]]
  //println("Making state: " + day)

  //Determine number of birds going to every other cell.

  //Now create dependent binomials
  //This creates more elements but smaller factors

  for (fromCell <- data.cells) {
   // println("Making transitions for day " + day)
    val reachableCells = data.getReachableFrom(fromCell)
   // println("Cells reachable from " + fromCell + ": " + reachableCells)
    val transitionProbabilities = reachableCells.map((toCell: Int) => makeTheta(fromCell, toCell))

    if (previousState.numberOfBirdsInCell.contains(fromCell)) {
      val transitioningFrom = Multinomial(previousState.numberOfBirdsInCell(fromCell), transitionProbabilities, reachableCells)

      for (toCell <- data.cells) {
        if (birdsTransitioningToCell.contains(toCell) && transitioningFrom.contains(toCell)) {
          birdsTransitioningToCell(toCell) += transitioningFrom(toCell)
        } else if (transitioningFrom.contains(toCell)) {
          birdsTransitioningToCell += toCell -> ListBuffer[Element[Int]](transitioningFrom(toCell))
        }
      }
    }

  }

  /*
    //Create N independent binomials.
  //Create a transition from every cell to every (reachable) cell 
  val birdsTransitioningToCell = mutable.Map.empty[Int, List[Element[Int]]]
  //println("Making state: " + day)
  for (toCell <- data.cells) {

    //Have to do another loop.
    //for toCell <- cells
    //previousState.numberOfBirdsInCell(cell)
    val transitionsFromOtherCells = ListBuffer.empty[Element[Int]]
    for (fromCell <- data.cells) {
    
      //Make a list buffer here
      //tolist it at the end of each iteration and add it to map.
      //then no need for contains, mutable map
      //println("theta: " + day + " " + fromCell + " " + toCell)
      if (data.isReachable(fromCell, toCell) == true) {
        //It should be possible to make this in one apply, instead of two.
        val theta = makeTheta(fromCell, toCell)
        //println(theta)
        val numberTransitioningIntoCellFrom: Element[Int] = Chain(previousState.numberOfBirdsInCell(fromCell), (i: Int) => Binomial(i, theta))
        transitionsFromOtherCells += numberTransitioningIntoCellFrom
        
      } else {
        println("Cell " + toCell + " not reachable from " + fromCell)
      }

    }

    birdsTransitioningToCell += toCell -> transitionsFromOtherCells.toList

  }
  */

  //This could be moved inside the first loop
  //I'll leave it as is for clarity.
  val numberOfBirdsInCell = mutable.Map.empty[Int, Element[Int]]
  for (cell <- data.cells) {

    val numberOfBirdsInCurrentCell: Element[Int] = Counter(birdsTransitioningToCell(cell).toList, data.numberOfBirds)

    val birdsObservedInCell = if (data.getBirdsObservedInCell(year, day, cell) > data.numberOfBirds) {
        data.getBirdsObservedInCell(year, day, cell)
    } else {
      data.getBirdsObservedInCell(year, day, cell)
    }
	  //println("birds observed in cell: " + cell + " on day " + day + ": " + birdsObservedInCell)
    //Note: This is a shortcut/
    //It is equivalent to creating a Poisson element and doing .observe with birdsObservedInCell
    //It's also required if we use a factored algorithm, because we don't have factors for Poisson.

      //val deviation = sqrt(data.numberOfBirds)
      numberOfBirdsInCurrentCell.setConstraint((n: Int) => {
        //val result = normalDensity(n.toDouble, birdsObservedInCell.toDouble,0.65) 
        val result = poissonDensity(n,birdsObservedInCell)
        println("set constraint (c,d,n,o,r): " + cell + " " + day + " " + n + " " + birdsObservedInCell + " " + result)
        result
      })
    

      
    //Add to map
    numberOfBirdsInCurrentCell.addPragma(Abstraction(5)(CounterAbstraction))
    numberOfBirdsInCell += cell -> numberOfBirdsInCurrentCell
  }

}

object ModelState {
/*
    //Remaining is the same as day.
  def transition(day: Int, previousState: Element[ModelState], data: ModelData): Element[ModelState] = {
    if (day == data.endDay) {
      println("End")
      previousState
    } else {
      println(day)
      CachingChain(previousState, (s: ModelState) => transition(day + 1, Constant(ModelState(s, s.data, s.year, s.day + 1)))) // s.next generates a new state given a prev
    }
  }
  */
  //Find number of states in model, which is fixed.
  def makeStates(day: Int, year: Int, data: ModelData): ModelState = {
    if (day == 1) {
      //Make initial state
      val s = new InitialState(year, day)(data)
      s
    } else {
      val s = new IntermediateState(makeStates(day - 1, year, data), year, day)(data)
      s
    }
  }

  def makeInitialState(data: ModelData, year: Int, day: Int): ModelState = {
    val s = new InitialState(year, day)(data)
    s
  }

  def makeNextState(previous: ModelState, data: ModelData, year: Int, day: Int): ModelState = {
    val s = new IntermediateState(previous, year, day)(data)
    s
  }

  def apply(data: ModelData, year: Int, day: Int) = makeInitialState(data, year, day)
  def apply(previousState: ModelState, data: ModelData, year: Int, day: Int) = makeNextState(previousState, data, year, day)
}
