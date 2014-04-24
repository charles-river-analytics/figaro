package com.cra.appril.birdmigration
import annotation.tailrec
import scala.math.{ exp, pow, ceil, log }
import JSci.maths.ExtraMath.factorial
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection._
import com.cra.appril.birdmigration.model.ModelData
import java.io._

class Generate(val data: ModelData, val b1: Double, val b2: Double, val b3: Double, val b4: Double, truthFileName: String, observedFileName: String) {

  val seed = System.currentTimeMillis()
  val r = new scala.util.Random(seed)
  //var timeSpentRandom:Long = 0
  //var invocations: Long = 0
  def generateGeometric(probFail: Double): Int = {
    val start = System.currentTimeMillis()
    val result = ceil(log(r.nextDouble()) / log(probFail)).toInt
    val end = System.currentTimeMillis()
    // timeSpentRandom += end - start
    //invocations += 1
    //if (timeSpentRandom % 10000 == 0) {
    //  println(timeSpentRandom + " " + invocations + " " + timeSpentRandom.toDouble/invocations.toDouble)

    //}  
    result
  }

  // Devroye, p. 525
  @tailrec
  private def binomialHelper(x: Int, sum: Int, n: Int, p: Double): Int = {
    val g = generateGeometric(1 - p)
    val newSum = sum + g
    val newX = x + 1
    if (newSum <= n) binomialHelper(newX, newSum, n, p)
    else newX
  }

  //r is a random number.
  def generateBinomial(n: Int, p: Double): Int = {
    if (p <= 0) 0; else if (p < 1) binomialHelper(-1, 0, n, p); else n
  }

  def normalize(unnormalized: List[Double]): List[Double] = {
    val normalizer = (0.0 /: unnormalized)(_ + _)
    if (normalizer == 0.0) throw new IllegalArgumentException
    unnormalized map (_ / normalizer)
  }

  def generateMultinomial(numberOfTrials: Int, outcomes: List[Int], probs: List[Double]): Map[Int, Int] = {
    val d = outcomes.size
    val unnormalized = probs
    val normalized = normalize(unnormalized)//Should not be necessary - However, we could postpone normalization to here if we wanted.
    var n = numberOfTrials
    var sum = 1.0 //The book says 0, which is surely a typo.

    for (p <- probs) {
      if (p > .1) {
        println("wow: " + p)
      }
    }
    
    val numberOfSuccesses = mutable.Map.empty[Int, Int]
	 for (o <- outcomes) {
	   numberOfSuccesses += o -> 0
	 }
    //It may actually be possible to write this as a Scala one-liner.
    for (i <- 0 to d) {
      val p = normalized(i)
      numberOfSuccesses(outcomes(i)) = generateBinomial(n, p / sum)
      n = n - numberOfSuccesses(outcomes(i))
      if (n <= 0) {
        //println("Sum of entries: " + randomVector.sum)
        return numberOfSuccesses
      }
      sum = sum - normalized(i)
    }
    //println("Sum of entries: " + randomVector.sum)
    numberOfSuccesses
  }

  def generatePoisson(lambda: Double): Int = {

    val expMinusLambda = exp(-lambda)

    @tailrec
    def generateHelper(x: Int, prod: Double): Int = {
      val newProd = prod * r.nextDouble()
      if (newProd > expMinusLambda) generateHelper(x + 1, newProd)
      else x
    }

    val result = generateHelper(0, 1)
    result
  }

  //For a pair of cells, this is the same in every day/year
  //It's also the same across datasets of the same size
  //so for making our own 4x4/10x10 we can borrow theirs

  //This is clearly the grid distance scaled from 0 to 1.

  def wackyDistance(x: Int, y: Int): Int = {
    //
    //val gridDistance = gridDistance(x,y)

    if (x == y) {
      0
    }
    0
  }

  def vectorDistance(x: Int, y: Int): Int = {
    if (x == y) {
      0
    }
    0
  }

  //choose NSEW uniformly.
  //value is chosen based on angle between wind vector and direction vector.
  def wind(w: Double, d: Double): Double = {
    //Cosine of angle
    //First make vector
    //then find angle
    //then cosine of angle
    val angle = 0.0
    Math.cos(angle)
  }

  def makeTheta(fromCell: Int, toCell: Int, year: Int, night: Int): Double = {
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

    result
  }

  def getCellsReachableFrom(cell: Int): List[Int] = {
    List(0)
  }

  val header = "Year,Day,Cell1,Cell2,Cell3,Cell4,Cell5,Cell6,Cell7,Cell8,Cell9,Cell10,Cell11,Cell12,Cell13,Cell14,Cell15,Cell16,Cell17,Cell18,Cell19,Cell20,Cell21,Cell22,Cell23,Cell24,Cell25,Cell26,Cell27,Cell28,Cell29,Cell30,Cell31,Cell32,Cell33,Cell34,Cell35,Cell36,Cell37,Cell38,Cell39,Cell40,Cell41,Cell42,Cell43,Cell44,Cell45,Cell46,Cell47,Cell48,Cell49,Cell50,Cell51,Cell52,Cell53,Cell54,Cell55,Cell56,Cell57,Cell58,Cell59,Cell60,Cell61,Cell62,Cell63,Cell64,Cell65,Cell66,Cell67,Cell68,Cell69,Cell70,Cell71,Cell72,Cell73,Cell74,Cell75,Cell76,Cell77,Cell78,Cell79,Cell80,Cell81,Cell82,Cell83,Cell84,Cell85,Cell86,Cell87,Cell88,Cell89,Cell90,Cell91,Cell92,Cell93,Cell94,Cell95,Cell96,Cell97,Cell98,Cell99,Cell100"

  def generate = {

    val truthFile: java.io.File = new File(truthFileName)
    val obsFile: java.io.File = new File(observedFileName)
    val truthWriter = new java.io.PrintWriter(truthFile)
    val obsWriter = new java.io.PrintWriter(obsFile)

    println("Generating.")
    try {
      //Write header - Column names
      obsWriter.write(header)
      obsWriter.write(System.lineSeparator())
      truthWriter.write(header)
      truthWriter.write(System.lineSeparator())
      println("Wrote header.")

      for (year <- data.years) {
        println("year: " + year)
        var currentBirdsInCell = Array.fill(data.cells.length)(0)
        var prevBirdsInCell = Array.fill(data.cells.length)(0)
        currentBirdsInCell(0) = data.numberOfBirds
        for (day <- data.days) {
          
         // println("day " + day)
          obsWriter.write(year + "," + day)
          truthWriter.write(year + "," + day)


          //All birds begin in bottom left

          //Do this first.
          for (cell <- data.cells) {
            val observed = generatePoisson(currentBirdsInCell(cell - 1).toDouble) //May have some noise.
            val trueNumber = currentBirdsInCell(cell - 1)
            obsWriter.write("," + observed)
            truthWriter.write("," + trueNumber)
          }
          obsWriter.write(System.lineSeparator())
          truthWriter.write(System.lineSeparator())
          prevBirdsInCell = currentBirdsInCell
          currentBirdsInCell = Array.fill(data.cells.length)(0)

          if (day < data.endDay) {
        	//println("transition")
            for (fromCell <- data.cells) {
              //Make conditions for all
              //but not probs.
                //println("cell" + toCell)
                //If this cell has birds in it, we must decide where they go.
                if (prevBirdsInCell(fromCell - 1) > 0) {
                  val reachableCells = data.getReachableFrom(fromCell)
                  val t = reachableCells.map((toCell: Int) => makeTheta(fromCell, toCell, year, day))
                  //this just has to be summed up in the next step.
                  val birdsTransitioning = generateMultinomial(prevBirdsInCell(fromCell - 1), reachableCells, t)
                  //This is wrong. The index doens't necessarily match
                  //currentBirdsInCell is an array of 0 to 100, birds transitioning is just the cells reachable.
                  
                  //This is still wrong
                  //we need to loop through every key
                //  println(fromCell)
                //  println(reachableCells.sorted)
                 // println(birdsTransitioning.keys.toList.sorted)
                  for (k <- birdsTransitioning.keys) {
                	 // println(k)
                	  currentBirdsInCell(k - 1) += birdsTransitioning(k)  
                  }
                  
                }

            }

          }
        }
      }
      
      println("done.")
      
    } finally {
      truthWriter.close
      obsWriter.close
    }
  }

  //For now, we reuse the original features and make new observations
  /*
  def generate = {
    val numberOfBirdsInProblemInstance = 1000
    val gridDimension = 10
    val numberOfYears = 30
    val truthFileName = 0
    val parameterFileName = 0
    val observationsFileName = 0
    val featureFileName = 0
    val years = List(1, 2, 3)
    val days = List(1, 2, 3)//Replace with range.
    val cells = List(1, 2, 3)//Replace with range

    //Generate parameters
    //Uniform random - Could generate from Normal instead
    val b1 = r.nextDouble
    val b2 = r.nextDouble
    val b3 = r.nextDouble
    val b4 = r.nextDouble
    
    val birdsInCell = Array.fill(cells.length)(0)
    birdsInCell(0) = numberOfBirdsInProblemInstance
    //val birdsInCell //Array or map

    //First do conditions
    for (year <- years) {
      for (day <- days) {
        
        val windDirection = 0.0
        for (fromCell <- cells) {
          //Make conditions for all
          //but not probs.
          for (toCell <- cells) {
            val x_1 = wackyDistance(fromCell, toCell)
            val x_2 = vectorDistance(fromCell, toCell)
            //Could generate from Normal instead
            //Or even do R analysis as Glenn suggested.
            val direction = 0.0
            val x_3 = wind(windDirection,direction)
            val x_4 = if (fromCell == toCell) 1 else 0
            if (birdsInCell(fromCell - 1) > 0) {
              //make probs.
              //Needs all xs for fromcell. These need to be generated before.
              val t = makeTheta(fromCell, toCell)

              //this just has to be summed up in the next step.
              val birdsTransitioning = generateMultinomial(birdsInCell(fromCell - 1), cells, t)
              birdsInCell(toCell) += birdsTransitioning(toCell)
            }

          }
        }
      }
    }

    //Now do observations from truth above.
    for (year <- years) {
      for (day <- days) {
        for (cell <- cells) {
            //Not exactly right, since we have to get the number of birds on a given day.
        	val observed = generatePoisson(birdsInCell(cell).toDouble)
        }
      }
    }
   }
*/

}