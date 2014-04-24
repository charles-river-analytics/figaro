package com.cra.appril.birdmigration

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Apply
import com.cra.figaro.library.atomic._
import com.cra.appril.birdmigration.data.io.DataReader
import com.cra.appril.birdmigration.model.Model
import com.cra.appril.birdmigration.model.ModelData
import com.cra.appril.birdmigration.model.ParameterModel
import com.cra.figaro.algorithm.Abstraction
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.language.Constant

class Main {

}

object Main {
  
/*
  def testNew = {
    val x1 = discrete.Uniform(2.0,4.0,6.0)
    val a1 = Apply(x1,(d1:Double) => d1*2.0)
    val a2 = Apply(x1,(d1:Double) => d1*2.0)
    //a1.observe(4.0)//Expectation of 2.0
    //a2.observe(12.0)//This line will result in an expectation of 0 or NaN
    val t = List(a1,x1)
    val inferenceAlgorithm = BeliefPropagation.lazyBP(10,600,false,t:_*)
    //val inferenceAlgorithm = VariableElimination(t:_*)
    inferenceAlgorithm.start
    val e = inferenceAlgorithm.expectation(x1, (d1:Double) => d1)
    println(e)
    inferenceAlgorithm.stop
    inferenceAlgorithm.kill
  }
 */ 
  def main() :Unit = {
    main(List().toArray)
  }
  
  def main(args: Array[String]): Unit = {


    //val observationsFileName = "onebird-observations.csv"
   // val featuresFileName = "onebird_features.csv"
    
    
    //val observationsFileName = "R:/BirdModel/cp2-birdcast-v1/onebird/onebird-observations.csv"
    //val featuresFileName = "R:/BirdModel/cp2-birdcast-v1/onebird/onebird_features.csv"
    
   // val observationsFileName = "R:/BirdModel/cp2-birdcast-v1/thousandbird/10x10x1000-train-observations.csv"
  // val featuresFileName ="R:/BirdModel/cp2-birdcast-v1/thousandbird/10x10x1000-features.csv"
  //  val obsFileName= "R:/BirdModel/cp2-birdcast-v1/thousandbird/10x10x1000-observations_custom.csv"
  //  val truthFileName = "R:/BirdModel/cp2-birdcast-v1/thousandbird/10x10x1000-observations_custom_truth.csv"
    val observationsFileName = "10x10x1000-train-observations.csv"
    val featuresFileName ="10x10x1000-features.csv"
    
    //val observationsFileName = "onebird-observations.csv"
    //val featuresFileName ="onebird_features_corrected.csv"
    
    //val observationsFileName = "10x10x1000000-train-observations.csv"
    //val featuresFileName = "10x10x1000000-train-features.csv"
    //val observationsFileName = "R:/BirdModel/cp2-birdcast-v1/millionbird/10x10x1000000-train-observations.csv"
    //val featuresFileName = "R:/BirdModel/cp2-birdcast-v1/millionbird/10x10x1000000-train-features_corrected.csv"
    val gridDimension = 10//How many cells to expect  
    val reader = DataReader(gridDimension)
    val birdsObservedInCell = reader.readObservationsFile(observationsFileName)
    val featureInCell = reader.readFeaturesFile(featuresFileName)

    
    //2
   // val b1 = discrete.Uniform(.20,0.40,0.60,0.80)
    //val b2 = discrete.Uniform(.20,0.40,0.60,0.80)
   // val b3 = discrete.Uniform(.20,0.40,0.60,0.80)
  //  val b4 = discrete.Uniform(.20,0.40,0.60,0.80)
    
    //val b1 = discrete.Uniform(.20,0.80)
   // val b2 = discrete.Uniform(.20,0.80)
   // val b3 = discrete.Uniform(.20,0.80)
   // val b4 = discrete.Uniform(.20,0.80)
    val b1 = continuous.Uniform(1.0,20.0)
    val b2 = continuous.Uniform(1.0,20.0)
    val b3 = continuous.Uniform(1.0,20.0)
    val b4 = continuous.Uniform(1.0,20.0)
    //Make all of these parameters
    val numberOfBirds = 1000
    val startDay = 1
    val endDay = 20//20 days, 19 nights.
    val startYear = 1
    val endYear = 3
   
    val data = ModelData(gridDimension, numberOfBirds,startYear,endYear, startDay,endDay,b1,b2,b3,b4,birdsObservedInCell,featureInCell)
    
    //val model = Model(data)
    val model = new ParameterModel()
    model.doModel(data)
    
    //val g = new Generate(data,7.5,5,5,20.0,truthFileName,obsFileName)
   // g.generate
    //model.doM
    //model.runEstimationTransition
    
   

  }
}