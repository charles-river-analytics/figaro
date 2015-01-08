/*
 * OpenUniverseLearning.scala
 * An example of open universe learning.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Jan 1, 2009
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.example

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.util._
import java.io._
import scala.math.abs

/**
 * Open universe learning.
 */
object OpenUniverseLearning {
  //This will by default output to the location from which this test file is executed. 
  //If you would like to place the output in a different directory, specify it here.
  val directory = ""
  val filename = directory + "Open Universe Learning Results.csv"
  def numEMIterations = 1
  val minTrainingSetSize = 10
  val maxTrainingSetSize = 10
  val trainingSetSizeStep = 10
  val randomSeed = -1
  
  random.setSeed(randomSeed)

  val maxAircraft1 = 5
  val maxAircraft2 = 3
  val probContinue1 = 0.6
  val probContinue2 = 0.4
  val probObserve1 = 0.3
  val probObserve2 = 0.2
  val probIncorrectObservation1 = 0.4
  val probIncorrectObservation2 = 0.2
  
  val betaContinue1 = BetaParameter(1,1)
  val betaContinue2 = BetaParameter(1,1)
  val betaObserve1 = BetaParameter(1,1)
  val betaObserve2 = BetaParameter(1,1)
  
  sealed abstract class Observation
  case object Aircraft1 extends Observation
  case object Aircraft2 extends Observation
  case object Aircraft3 extends Observation
  
  class Model(continue1: () => Element[Boolean], continue2: () => Element[Boolean], observe1: () => Element[Boolean], observe2: () => Element[Boolean]) {
    def makeExistsArray(maxNumAircraft: Int, continue: () => Element[Boolean]): Array[Element[Boolean]] = {
      val result: Array[Element[Boolean]] = Array.fill(maxNumAircraft)(null)
      result(0) = continue()
      for { i <- 1 until maxNumAircraft } { result(i) = If(result(i-1), continue(), Constant(false)) }
      result
    }
    val exists1 = makeExistsArray(maxAircraft1, continue1)
    val exists2 = makeExistsArray(maxAircraft2, continue2)
    
    def makeObservedArray(existsArray: Array[Element[Boolean]], observe: () => Element[Boolean]): Array[Element[Boolean]] = {
      for { existence <- existsArray } yield If(existence, observe(), Constant(false))
    }
    val observed1 = makeObservedArray(exists1, observe1)
    val observed2 = makeObservedArray(exists2, observe2)
  
    def makeObservationArray(observedArray: Array[Element[Boolean]], correct: Observation, prob3: () => Element[Boolean]): Array[Element[Observation]] = {
      for { observed <- observedArray } yield {
        If(prob3(), Aircraft3, correct)
      }
    }
    val observations1 = makeObservationArray(observed1, Aircraft1, () => Flip(probIncorrectObservation1))
    val observations2 = makeObservationArray(observed1, Aircraft1, () => Flip(probIncorrectObservation2))

    def count(observedArray: Array[Element[Boolean]], observationArray: Array[Element[Observation]], aircraft: Observation): Element[Int] = {
      val size = observedArray.size
      val countArray: Array[Element[Int]] = Array.fill(size+1)(null)
      countArray(0) = Constant(0)
      for { i <- 1 to size } {
        val entry = Apply(observedArray(i-1), observationArray(i-1), (b: Boolean, o: Observation) => if (b && o == aircraft) 1; else 0)
        countArray(i) = Apply(countArray(i-1), entry, (i: Int, j: Int) => i + j)
      }
      countArray(size)
    }
    
    val count11 = count(observed1, observations1, Aircraft1)
    val count13 = count(observed1, observations1, Aircraft3)
    val count22 = count(observed2, observations2, Aircraft2)
    val count23 = count(observed2, observations2, Aircraft3)
    val total3 = Apply(count13, count23, (i: Int, j: Int) => i + j)
  }  
  
  class TrueModel extends Model(() => Flip(probContinue1), () => Flip(probContinue2), () => Flip(probObserve1), () => Flip(probObserve2))
  class LearningModel extends Model(
      () => new ParameterizedFlip("", betaContinue1, universe), 
      () => new ParameterizedFlip("", betaContinue2, universe), 
      () => new ParameterizedFlip("", betaObserve1, universe), 
      () => new ParameterizedFlip("", betaObserve2, universe))
   
  class LearnedModel(pContinue1: Double, pContinue2: Double, pObserve1: Double, pObserve2: Double) 
  extends Model(() => Flip(pContinue1), () => Flip(pContinue2), () => Flip(pObserve1), () => Flip(pObserve2))
  
  case class Datum(count1: Int, count2: Int, count3: Int)
  
  def generate(): Datum = {
    val model = new TrueModel
    Datum(model.count11.value, model.count22.value, model.total3.value)
  }
  
  def observe(model: Model, datum: Datum) {
    model.count11.observe(datum.count1)
    model.count22.observe(datum.count2)
    model.total3.observe(datum.count3)
  }
  
  def train(trainingSet: List[Datum]): Model = {
    for (datum <- trainingSet) observe(new LearningModel, datum)
    
    val time0 = System.currentTimeMillis()
    val algorithm = EMWithVE(numEMIterations, betaContinue1, betaContinue2, betaObserve1, betaObserve2)
    algorithm.start()

    val resultUniverse = new Universe
    val learnedPContinue1 = betaContinue1.expectedValue
    val learnedPContinue2 = betaContinue2.expectedValue
    val learnedPObserve1 = betaObserve1.expectedValue
    val learnedPObserve2 = betaObserve2.expectedValue
    algorithm.kill()
    val time1 = System.currentTimeMillis()
    println("Training time: " + ((time1 - time0) / 1000.0) + " seconds")
    new LearnedModel(learnedPContinue1, learnedPContinue2, learnedPObserve1, learnedPObserve2)
  }
  
  def assessModel(continue1: Element[Double], continue2: Element[Double], observe1: Element[Double], observe2: Element[Double]): (Double, Double, Double, Double, Double) = {
    val continue1Error = math.abs(continue1.value - probContinue1)
    val continue2Error = math.abs(continue2.value - probContinue2)
    val observe1Error = math.abs(observe1.value - probObserve1)
    val observe2Error = math.abs(observe2.value - probObserve2)
    val average = (continue1Error + continue2Error + observe1Error + observe2Error) / 4
    println("Continue1 error = " + continue1Error)
    println("Continue2 error = " + continue2Error)
    println("Observe1 error = " + observe1Error)
    println("Observe2 error = " + observe2Error)
    println("Average parameter error = " + average)
    (continue1Error, continue2Error, observe1Error, observe2Error, average)
  }
  
  def experiment(stream: BufferedWriter, numTrainingExamples: Int) {

    println(numTrainingExamples.toString + " training examples")
    val trainingSet = List.fill(numTrainingExamples)(generate())
    
    println("Learning:")

    val learnedModel = train(trainingSet)
    val (continue1Err, continue2Err, observe1Err, observe2Err, averageErr) = assessModel(betaContinue1, betaContinue2, betaObserve1, betaObserve2)

    stream.write(numTrainingExamples.toString + ", " + continue1Err + ", " + continue2Err + ", " + observe1Err + ", " + observe2Err + ", " + averageErr + "\n")
    stream.flush()
  }

  def main(args: Array[String]) {
    val time0 = System.currentTimeMillis()
    val file = new File(filename)
    val outputStream = new BufferedWriter(new FileWriter(file))
    outputStream.write(", Learning Parameter Error, MCMC Parameter Error, True Prediction Accuracy, Learning Prediction Accuracy, MCMC Prediction Accuracy\n")
    outputStream.flush()
    for { i <- minTrainingSetSize to maxTrainingSetSize by trainingSetSizeStep } {
      experiment(outputStream, i)
    }
    outputStream.close()
    val time1 = System.currentTimeMillis()
    println("Done: total time = " + ((time1 - time0) / 1000.0))
  }
}
