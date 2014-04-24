package com.cra.appril.birdmigration.model

import scala.collection.mutable.ListBuffer
import scala.math.ceil
import scala.math.sqrt

import com.cra.appril.birdmigration.data.CellFeatures
import com.cra.figaro.language.Element
import com.cra.figaro.library.atomic._
import scala.collection._

//Methods for accessing data.
//Parameters possibly stored as separete class
//MakeTheta could go in here as well.
class ModelData(val gridDimension: Int, val numberOfBirds: Int, val startYear: Int, val endYear: Int, val startDay: Int, val endDay: Int, val beta1: Element[Double], val beta2: Element[Double], val beta3: Element[Double], val beta4: Element[Double], val birdsInCell: Map[Int, Int], val featuresBetweenCells: Map[Int, CellFeatures]) {

  //Careful with indices.
  val years = Seq.range(startYear, endYear + 1) //Range does not include upper bound
  val days = Seq.range(startDay, endDay + 1) //Range does not include upper bound
  val nights = Seq.range(startDay, endDay)
  val distanceLimit = sqrt(18)
  val parameters = Seq(beta1, beta2, beta3, beta4)
  val cells = Seq.range(1, (gridDimension * gridDimension) + 1) //1 to 16 (.range does not include upper bound)

  def cleanUp() {
    
    
  }
  
  

  
  def angleBetweenCells(cell1: Int, cell2: Int): Double = {
    //Get row/column to determine x and y
    //Then use arctangent to determine angle between x_1, y_1 and x_2,y_2
    //Then assume wind is discretized
    //then compute angle between wind and cell direction
    //from this we can also determine the actual values for wind direction
    //and find the distribution.
    0.0
  }
  
  //We want:
  //1 -> 1
  //2 -> 2
  //3 -> 3
  //4 -> 4
  //5 -> 1
  //...
  //16 -> 4
  //Could also do zip continous stream (1,2,3,4) with 1 - 16 and store in map.
  def rowOfCell(cell: Int): Int = {
    val row = (cell + (gridDimension - 1)) % gridDimension + 1
    row
  }

  //We want:
  //1 -> 1
  //4 -> 1
  //5 -> 2
  //16 -> 4
  //Need to test these methods.
  def columnOfCell(cell: Int): Int = {
    val column = ceil(cell.toDouble / gridDimension.toDouble).toInt
    column
  }

  def gridDistance(fromCell: Int, toCell: Int): Double = {
    val cellsVertical = rowOfCell(fromCell) - rowOfCell(toCell)
    val cellsHorizontal = columnOfCell(fromCell) - columnOfCell(toCell)
    val gridDistance = sqrt((cellsVertical * cellsVertical) + (cellsHorizontal * cellsHorizontal))
    gridDistance
  }

  //This never needs to be recomputed. It's the same in every year/day
  //Just cache it
  //Or even hard code it.
  
  case class Memo[A,B](f: A => B) extends (A => B) {
  private val cache = mutable.Map.empty[A, B]
  def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }
  
  val getReachableFrom: Memo[Int, List[Int]] = Memo {
    case fromCell => { 
	    val result = ListBuffer.empty[Int]
	    //Make sure inclusive.
	    for (toCell <- 1 to gridDimension*gridDimension) {
	      if (isReachable(fromCell,toCell)) {
	        result += toCell
	      }
	    }
	    result.toList
    }
  }

  def isReachable(fromCell: Int, toCell: Int): Boolean = {
    val d = gridDistance(fromCell, toCell)
    if (gridDistance(fromCell, toCell) <= distanceLimit == true) {
      true
    } else {
      false
    }
  }

  //This quick method will break for larger grids (ie, more than 65,000 cells)
  def getBirdsObservedInCell(year: Int, day: Int, cell: Int): Int = {
    val key = (year * 10000) + (day * 100) + cell
    //println("y: " + year + " d: "  + day + " c2: " + cell + " k: " + key)
    birdsInCell(key)
  }



  def getFeaturesBetweenCells(year: Int, night: Int, fromCell: Int, toCell: Int): CellFeatures = {
    val key = (year * 1000000) + (night * 10000) + index(fromCell, toCell)
    featuresBetweenCells(key)
  }

  //We don't have to store this year*day times. It doesn't change.
  def getDistanceBetweenCells(year: Int, night: Int, fromCell: Int, toCell: Int): Double = {

    val key = (year * 1000000) + (night * 10000) + index(fromCell, toCell)
    if (night >= 19) {
    	//println("y: " + year + " d: " + night + " c1: " + fromCell + " c2: " + toCell + " k: " + key)
    }
    featuresBetweenCells(key).distance
  }

  //We don't have to store this year*day times. It doesn't change.
  def getVectorDifferenceBetweenCells(year: Int, night: Int, fromCell: Int, toCell: Int): Double = {
    val key = (year * 1000000) + (night * 10000) + index(fromCell, toCell)
    featuresBetweenCells(key).vectorDifference
  }

  //This is really the only thing that changes
  def getWindDirectionBetweenCells(year: Int, night: Int, fromCell: Int, toCell: Int): Double = {
    val key = (year * 1000000) + (night * 10000) + index(fromCell, toCell)
    featuresBetweenCells(key).windDirection
  }
  
  //We don't have to store this year*day times. It doesn't change.
  def getSameCellBetweenCells(year: Int, night: Int, fromCell: Int, toCell: Int): Int = {
    val key = (year * 1000000) + (night * 10000) + index(fromCell, toCell)
    if (featuresBetweenCells(key).fromEqualsTo) 2 else 0
  }

  def index(fromCell: Int, toCell: Int): Int = {
    ((fromCell << 16) ^ toCell)
  }

}

object ModelData {

  def makeModel(birdsInCell: Map[Int, Int], featuresBetweenCells: Map[Int, CellFeatures], b1: Element[Double], b2: Element[Double], b3: Element[Double], b4: Element[Double]) = {
    val gridDimension = 4
    val numberOfBirds = 1
    val startDay = 1
    val endDay = 20 //20 days, 19 nights.
    val startYear = 1
    val endYear = 3
    new ModelData(gridDimension, numberOfBirds, startDay, endDay, startYear, endYear, b1, b2, b3, b4, birdsInCell, featuresBetweenCells)
  }

  
  def makeModel(gridDimension: Int, numberOfBirds: Int, startYear: Int, endYear: Int, startDay: Int, endDay: Int, b1: Element[Double], b2: Element[Double], b3: Element[Double], b4: Element[Double], birdsInCell: Map[Int, Int], featuresBetweenCells: Map[Int, CellFeatures]) = {
     new ModelData(gridDimension, numberOfBirds, startYear, endYear, startDay, endDay, b1, b2, b3, b4, birdsInCell, featuresBetweenCells)
  }
  

  def apply(gridDimension: Int, numberOfBirds: Int, startYear: Int, endYear: Int, startDay: Int, endDay: Int, b1: Element[Double], b2: Element[Double], b3: Element[Double], b4: Element[Double], birdsInCell: Map[Int, Int], featuresBetweenCells: Map[Int, CellFeatures]) = {
    makeModel(gridDimension, numberOfBirds, startYear, endYear, startDay, endDay, b1, b2, b3, b4, birdsInCell, featuresBetweenCells)
  }
}