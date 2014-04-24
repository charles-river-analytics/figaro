package com.cra.appril.birdmigration.test
import scala.math.sqrt

import com.cra.figaro.library.atomic._
import com.cra.appril.birdmigration.data.io.DataReader
import com.cra.appril.birdmigration.model.ModelData
class ModelDataTest {

}

object modelDataTest {
  
  
  def IsReachableTest = {
	val observationsFileName = "R:/BirdModel/cp2-birdcast-v1/thousandbird/10x10x1000-train-observations.csv"
    val featuresFileName = "R:/BirdModel/cp2-birdcast-v1/thousandbird/10x10x1000-features.csv"

    
    val gridDimension = 10//How many cells to expect  
    val reader = DataReader(gridDimension)
    val birdsObservedInCell = reader.readObservationsFile(observationsFileName)
    val featureInCell = reader.readFeaturesFile(featuresFileName)
    val numberOfBirds = 1
    val startDay = 1
    val endDay = 3//20 days, 19 nights.
    val startYear = 1
    val endYear = 2
    val b1 = discrete.Uniform(0.25,0.50,0.75)
    val b2 = discrete.Uniform(0.25,0.50,0.75)
    val b3 = discrete.Uniform(0.25,0.50,0.75)
    val b4 = discrete.Uniform(0.25,0.50,0.75)
    val data = ModelData(gridDimension, numberOfBirds, startDay,endDay,startYear,endYear,b1,b2,b3,b4,birdsObservedInCell,featureInCell)
    val reachable = data.getReachableFrom(1)
    val actual = List(1,2,3,4,5,
                      11,12,13,14,15,
                      21,22,23,24,
                      31,32,33,
                      41,42
                      )
     println(3*sqrt(2))
     println("1,1 " + data.gridDistance(1,1))
     println("1,2 " + data.gridDistance(1,2))
     println("1,3 " + data.gridDistance(1,3))
     println("1,4 " + data.gridDistance(1,4))
     println("1,5 " + data.gridDistance(1,5))
     println("1,6 " + data.gridDistance(1,6))
     println("1,7 " + data.gridDistance(1,7))
     println("1,8 " + data.gridDistance(1,8))
     println("1,9 " + data.gridDistance(1,9))
     println("1,10 " + data.gridDistance(1,10))
     println("1,11 " + data.gridDistance(1,11))
     println("1,12 " + data.gridDistance(1,12))
     println("1,13 " + data.gridDistance(1,13))
     println("1,14 " + data.gridDistance(1,14))
                      
                      
     for (cell <- reachable) {
       if (actual.contains(cell) == false) { println(cell + " " + actual.contains(cell))}
     }
	
	     for (cell <- actual) {
       if (reachable.contains(cell) == false) { println(cell + " " + reachable.contains(cell))}
     }
	
	println
	val reachable45 = data.getReachableFrom(45)
	val actual45 = List( 39,49,59,
						 28,38,48,58,68,
						 17,27,37,47,57,67,77,
						 6,16,26,36,46,56,66,76,86,
						 5,15,25,35,45,55,65,75,85,
						 4,14,24,34,44,54,64,74,84,
						 13,23,33,43,53,63,73,
						 22,32,42,52,62,
						 31,41,51)
         for (cell <- reachable45) {
       if (actual45.contains(cell) == false)
         {println(cell + " " + actual45.contains(cell))
         
         }
     }
	
	   for (cell <- actual45) {
       if (reachable45.contains(cell) == false)
         {println(cell + " " + reachable45.contains(cell))
         
         }
     }
	   
	   
	 val reachable67 = data.getReachableFrom(67)
	 val actual67 = List(50,60,70,80,90,
			 			 39,49,59,69,79,89,99,
			 			 28,38,48,58,68,78,88,98,
			 			 27,37,47,57,67,77,87,97,
			 			 26,36,46,56,66,76,86,96,
			 			 35,45,55,65,75,85,95,
			 			 44,54,64,74,84,
			 			 53,63,73
	 )
	 
	          for (cell <- reachable67) {
       if (actual67.contains(cell) == false)
         {println(cell + " " + actual67.contains(cell))
         
         }
     }
	
	   for (cell <- actual67) {
       if (reachable67.contains(cell) == false)
         {println(cell + " " + reachable67.contains(cell))
         
         }
     }
	 
	 
  }
  
 def main(args: Array[String]) : Unit = {
   IsReachableTest
 }
  
}