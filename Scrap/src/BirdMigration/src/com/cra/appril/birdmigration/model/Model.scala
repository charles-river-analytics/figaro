package com.cra.appril.birdmigration.model

import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Chain
import com.cra.figaro.language.Element
import com.cra.figaro.language.Constant
import scala.collection.mutable.ListBuffer
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.CachingChain



class Model(val data: ModelData) {

  //Remaining is the same as day.
  /*
  def transition(remaining: Int, previousState: Element[ModelState]): Element[ModelState] = {
    if (remaining == data.endDay) {
      println("End")
      previousState
    } else {
      println(remaining)
      CachingChain(previousState, (s: ModelState) => transition(remaining + 1, Constant(ModelState(s, s.data, s.year, s.day + 1)))) // s.next generates a new state given a prev
    }
  }
*/
  def runEstimationTransition = {
    println("Running new version.")
    val parameters = List(data.beta1, data.beta2,data.beta3, data.beta4)
    val targets = ListBuffer.empty[Element[_]]
    //All 30 years

    //Make all years a chain
    //Set a flag as new initial state.
    //Use lazy depth of 1 to 600.
    //Plot lower and upper bounds
    //Plot running time for each depth.
    
    
    val numberOfIterations = 20
    val depth = 60
    println(data.cells)
    //for (year <- data.years) {
    val year = 1
      val initial = ModelState(data,year,data.startDay)
      //val next = ModelState(initial, data, year, initial.day + 1)
      //val el = Constant(next)
      //val e : Element[ModelState]= transition(initial.day,Constant(ModelState(initial, initial.data, initial.year, initial.day + 1)))
      //val numBirds = List.tabulate(data.gridDimension*data.gridDimension)(f => Chain(e, (m: ModelState) => m.numberOfBirdsInCell(f + 1)))
      //}
    targets += data.beta1
    targets += initial.birdReachedDestination
    //targets ++= numBirds
    val t = targets.toList
    
    println("Estimating with parameters with lazy BP.")
    
    
    //Do states need to be stored in individual universes?
    //Do we need to store elements in states all in one universe?
    //Presently, universe/names are not used at all.
    //val numberOfElements = Universe.universe.activeElements.size
    //println("Number of elements: " + numberOfElements)
    
    val inferenceAlgorithm = MetropolisHastings(100000, ProposalScheme.default, t:_*)
    
    //Nonzero results at depth 40
    
    //d = 100
    //(0.35312959030740704,0.35312959030740704)
//(0.23588296519293525,0.23588296519293525)
//(0.2016995693340405,0.2016995693340405)
//(0.20928787516561714,0.20928787516561714)
//Expectation value of b1: 0.45342914587157357
    val startTime = System.currentTimeMillis()
    //val inferenceAlgorithm = BeliefPropagation.lazyBP(numberOfIterations,depth,true,t:_*)//(10, 20*30, false, t:_*)
    //val inferenceAlgorithm = VariableElimination.debugged(t:_*)
    
    //println("Number of nodes in factor graph: " + inferenceAlgorithm.factorGraph.getNodes.size)
    //data.cleanUp()


    
    inferenceAlgorithm.start()
    println("Done.")
    val endTime = System.currentTimeMillis()
    val elapsedTime = endTime - startTime
    println("Elapsed Time: " + elapsedTime)
   // val b1v1 = inferenceAlgorithm.probabilityBounds(data.beta1, 0.20)
   // val b1v2 = inferenceAlgorithm.probabilityBounds(data.beta1, 0.40)
  //  val b1v3 = inferenceAlgorithm.probabilityBounds(data.beta1, 0.60)
 //   val b1v4 = inferenceAlgorithm.probabilityBounds(data.beta1, 0.80)

  //  println(b1v1)
  //  println(b1v2)
  //  println(b1v3)
 //   println(b1v4)

    
    
    
   // val e1lower = (b1v1._1*0.20 + b1v4._1*0.80)
   //  val e1upper = (b1v1._2*0.20  + b1v4._2*0.80)
    //println("Estimated value of b1(lower bound): " + e1lower)
    //println("Estimated value of b1(upper bound): " + e1upper)
    println("Expectation value of b1: " + inferenceAlgorithm.expectation(data.beta1, (d:Double) => d))
    /*
    val b2v1 = inferenceAlgorithm.probabilityBounds(data.beta2, 0.25)
    val b2v2 = inferenceAlgorithm.probabilityBounds(data.beta2, 0.50)
    val b2v3 = inferenceAlgorithm.probabilityBounds(data.beta2, 0.75)
    
    val e2 = (b2v1._1 + b2v2._1 + b2v3._1)/3.0
    
    val b3v1 = inferenceAlgorithm.probabilityBounds(data.beta3, 0.25)
    val b3v2 = inferenceAlgorithm.probabilityBounds(data.beta3, 0.50)
    val b3v3 = inferenceAlgorithm.probabilityBounds(data.beta3, 0.75)
    
    val e3= (b3v1._1 + b3v2._1 + b3v3._1)/3.0
    
    val b4v1 = inferenceAlgorithm.probabilityBounds(data.beta4, 0.25)
    val b4v2 = inferenceAlgorithm.probabilityBounds(data.beta4, 0.50)
    val b4v3 = inferenceAlgorithm.probabilityBounds(data.beta4, 0.75)
    
    val e4 = (b4v1._1 + b4v2._1 + b4v3._1)/3.0
    
    println("Estimated value of b1(lower bound): " + e1lower)
    println("Estimated value of b1(upper bound): " + e1upper)
   // println("Estimated value of b2: " + e2)
   // println("Estimated value of b3: " + e3)
   // println("Estimated value of b4: " + e4)
  */
    inferenceAlgorithm.stop
    inferenceAlgorithm.kill
  
    
  }
/*
  def runEstimation = {
		  
    println("Running old version.")
    //Each state creates related elements.
    //for (year <- data.years) {
    val year = 1
      val initialState = ModelState(data, year, 1)
      var previousState = initialState
      //We have 20 days
      //but 19 nights.
      //Don't do transition on 20th day.
      for (day <- data.days) {
        if (day == 1) {
          //skip, we already made the initial model.
        } else {
          val nextState = ModelState(previousState, data, year, day)
          previousState = nextState
        }
      }
    //}

    //Note: At this point, all data read from CSV can be cleared if more memory is needed.
    //Running out of memory on laptop - Can try on DM workstation
    //Do states need to be stored in individual universes?
    //Do we need to store elements in states all in one universe?
    //Presently, universe/names are not used at all.
    val numberOfElements = Universe.universe.activeElements.size
    println("Numer of elements: " + numberOfElements)
    //val inferenceAlgorithm = BeliefPropagation()
    val inferenceAlgorithm = VariableElimination.debugged(previousState.numberOfBirdsInCell(1),data.beta1, data.beta2, data.beta3, data.beta4)//(10, 20*30, false, t:_*)
    //println("Number of nodes in factor graph: " + inferenceAlgorithm.factorGraph.getNodes.size)

    //val inferenceAlgorithm = MetropolisHastings(1000, ProposalScheme.default, data.beta1, data.beta2, data.beta3, data.beta4)
    //data.cleanUp()
    inferenceAlgorithm.start()

    val e1 = inferenceAlgorithm.expectation(data.beta1, (d: Double) => d)
    val e2 = inferenceAlgorithm.expectation(data.beta2, (d: Double) => d)
    val e3 = inferenceAlgorithm.expectation(data.beta3, (d: Double) => d)
    val e4 = inferenceAlgorithm.expectation(data.beta4, (d: Double) => d)

    println("Estimated value of b1: " + e1)
    println("Estimated value of b2: " + e2)
    println("Estimated value of b3: " + e3)
    println("Estimated value of b4: " + e4)

    inferenceAlgorithm.stop
    inferenceAlgorithm.kill
  }
  */

}

object Model {
  def apply(data: ModelData) = new Model(data)
}