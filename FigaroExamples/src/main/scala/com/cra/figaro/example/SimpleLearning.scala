/*
 * SimpleLearning.scala
 * Learning algorithm tests.
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
import com.cra.figaro.util._
import scala.math.abs
import java.io._

/**
 * Learning algorithm tests.
 */
object SimpleLearning {
  //This will by default output to the location from which this test file is executed. 
  //If you would like to place the output in a different directory, specify it here.
  val directory = ""
  val filename = directory + "Open Universe Learning Results.csv"
  val observationProbability = 0.7
  val trainingSetSize = 100
  val testSetSize = 100
  val randomSeed = -4
  val minScale = 100
  val maxScale = 100
  val scaleStep = 2
  val mHScaleFactor = 75

  random.setSeed(randomSeed)

  abstract class Parameters(val universe: Universe) {
    val b1: Element[Double]
    val b2: Element[Double]
    val b3: Element[Double]
    val b4: Element[Double]
    val b5: Element[Double]
    val b6: Element[Double]
    val b7: Element[Double]
    val b8: Element[Double]
    val b9: Element[Double]
  }

  val trueB1 = 0.1
  val trueB2 = 0.2
  val trueB3 = 0.3
  val trueB4 = 0.4
  val trueB5 = 0.5
  val trueB6 = 0.6
  val trueB7 = 0.7
  val trueB8 = 0.8
  val trueB9 = 0.9

  val trueUniverse = new Universe

  object TrueParameters extends Parameters(trueUniverse) {
    val b1 = Constant(trueB1)("b1", universe)
    val b2 = Constant(trueB2)("b2", universe)
    val b3 = Constant(trueB3)("b3", universe)
    val b4 = Constant(trueB4)("b4", universe)
    val b5 = Constant(trueB5)("b5", universe)
    val b6 = Constant(trueB6)("b6", universe)
    val b7 = Constant(trueB7)("b7", universe)
    val b8 = Constant(trueB8)("b8", universe)
    val b9 = Constant(trueB9)("b9", universe)
  }

  class BayesianParameters(universe: Universe) extends Parameters(universe) {
    val b1 = Beta(1, 1)("b1", universe)
    val b2 = Beta(1, 1)("b2", universe)
    val b3 = Beta(1, 1)("b3", universe)
    val b4 = Beta(1, 1)("b4", universe)
    val b5 = Beta(1, 1)("b5", universe)
    val b6 = Beta(1, 1)("b6", universe)
    val b7 = Beta(1, 1)("b7", universe)
    val b8 = Beta(1, 1)("b8", universe)
    val b9 = Beta(1, 1)("b9", universe)
  }

  class LearnableParameters(universe: Universe) extends Parameters(universe) {
    val b1 = BetaParameter(1, 1)("b1", universe)
    val b2 = BetaParameter(1, 1)("b2", universe)
    val b3 = BetaParameter(1, 1)("b3", universe)
    val b4 = BetaParameter(1, 1)("b4", universe)
    val b5 = BetaParameter(1, 1)("b5", universe)
    val b6 = BetaParameter(1, 1)("b6", universe)
    val b7 = BetaParameter(1, 1)("b7", universe)
    val b8 = BetaParameter(1, 1)("b8", universe)
    val b9 = BetaParameter(1, 1)("b9", universe)
  }

  var id = 0

  class Model(val parameters: Parameters, flipConstructor: (Element[Double], String, Universe) => Flip) {
    id += 1
    val universe = parameters.universe
    val x = flipConstructor(parameters.b1, "x_" + id, universe)
    val f2 = flipConstructor(parameters.b2, "f2_" + id, universe)
    val f3 = flipConstructor(parameters.b3, "f3_" + id, universe)
    val f4 = flipConstructor(parameters.b4, "f4_" + id, universe)
    val f5 = flipConstructor(parameters.b5, "f5_" + id, universe)
    val f6 = flipConstructor(parameters.b6, "f6_" + id, universe)
    val f7 = flipConstructor(parameters.b7, "f7_" + id, universe)
    val f8 = flipConstructor(parameters.b8, "f8_" + id, universe)
    val f9 = flipConstructor(parameters.b9, "f9_" + id, universe)
    val y = If(x, f2, f3)("y_" + id, universe)
    val z = If(x, f4, f5)("z_" + id, universe)
    val w = CPD(y, z, (true, true) -> f6, (true, false) -> f7,
      (false, true) -> f8, (false, false) -> f9)("w_" + id, universe)
  }

  def normalFlipConstructor(parameter: Element[Double], name: String, universe: Universe) = new CompoundFlip(name, parameter, universe)

  def learningFlipConstructor(parameter: Element[Double], name: String, universe: Universe) = {
    parameter match {
      case p: AtomicBeta => new ParameterizedFlip(name, p, universe)
      case _ => throw new IllegalArgumentException("Not a beta parameter")
    }
  }

  class TrueModel extends Model(TrueParameters, normalFlipConstructor)

  case class Datum(x: Boolean, y: Boolean, z: Boolean, w: Boolean)

  def generateDatum(): Datum = {
    val model = new TrueModel
    Datum(model.x.value, model.y.value, model.z.value, model.w.value)
  }

  def observe(model: Model, datum: Datum) {
    if (random.nextDouble() < observationProbability) model.x.observe(datum.x)
    if (random.nextDouble() < observationProbability) model.y.observe(datum.y)
    if (random.nextDouble() < observationProbability) model.z.observe(datum.z)
    if (random.nextDouble() < observationProbability) model.w.observe(datum.w)
  }

  var nextSkip = 0

  def predictionAccuracy(model: Model, datum: Datum): Double = {
    model.x.unobserve()
    model.y.unobserve()
    model.z.unobserve()
    model.w.unobserve()
    val result = nextSkip match {
      case 0 =>
        model.y.observe(datum.y)
        model.z.observe(datum.z)
        model.w.observe(datum.w)
        val alg = MetropolisHastings(20000, ProposalScheme.default(model.universe), model.x)(model.universe)
        alg.start()
        alg.probability(model.x, datum.x)
      case 1 =>
        model.x.observe(datum.x)
        model.z.observe(datum.z)
        model.w.observe(datum.w)
        val alg = MetropolisHastings(20000, ProposalScheme.default(model.universe), model.y)(model.universe)
        alg.start()
        alg.probability(model.y, datum.y)
      case 2 =>
        model.x.observe(datum.x)
        model.y.observe(datum.y)
        model.w.observe(datum.w)
        val alg = MetropolisHastings(20000, ProposalScheme.default(model.universe), model.z)(model.universe)
        alg.start()
        alg.probability(model.z, datum.z)
      case 3 =>
        model.x.observe(datum.x)
        model.y.observe(datum.y)
        model.z.observe(datum.z)
        val alg = MetropolisHastings(20000, ProposalScheme.default(model.universe), model.w)(model.universe)
        alg.start()
        alg.probability(model.w, datum.w)
    }
    nextSkip = (nextSkip + 1) % 4
    result
  }

  def parameterError(model: Model): Double = {
    val parameters = model.parameters

    (abs(parameters.b1.value - trueB1) + abs(parameters.b2.value - trueB2) + abs(parameters.b3.value - trueB3) +
      abs(parameters.b4.value - trueB4) + abs(parameters.b5.value - trueB5) + abs(parameters.b6.value - trueB6) +
      abs(parameters.b7.value - trueB7) + abs(parameters.b8.value - trueB8) + abs(parameters.b9.value - trueB9)) / 9.0
  }

  def assessModel(model: Model, testSet: Seq[Datum]): (Double, Double) = {
    val paramErr = parameterError(model)
    nextSkip = 0
    var totalPredictionAccuracy = 0.0
    for (datum <- testSet) (totalPredictionAccuracy += predictionAccuracy(model, datum))
    val predAcc = totalPredictionAccuracy / testSet.length
    (paramErr, predAcc)
  }

  def train(trainingSet: List[Datum], parameters: Parameters, algorithmCreator: Parameters => Algorithm, valueGetter: (Algorithm, Element[Double]) => Double,
    flipConstructor: (Element[Double], String, Universe) => Flip): (Model, Double) = {
    for (datum <- trainingSet) observe(new Model(parameters, flipConstructor), datum)

    val time0 = System.currentTimeMillis()
    val algorithm = algorithmCreator(parameters)
    algorithm.start()

    val resultUniverse = new Universe
    def extractParameter(parameter: Element[Double], name: String) = Constant(valueGetter(algorithm, parameter))(name, resultUniverse)
    val learnedParameters = new Parameters(resultUniverse) {
      val b1 = extractParameter(parameters.b1, "b1")
      val b2 = extractParameter(parameters.b2, "b2")
      val b3 = extractParameter(parameters.b3, "b3")
      val b4 = extractParameter(parameters.b4, "b4")
      val b5 = extractParameter(parameters.b5, "b5")
      val b6 = extractParameter(parameters.b6, "b6")
      val b7 = extractParameter(parameters.b7, "b7")
      val b8 = extractParameter(parameters.b8, "b8")
      val b9 = extractParameter(parameters.b9, "b9")
    }

    algorithm.kill()
    val time1 = System.currentTimeMillis()
    val totalTime = (time1 - time0) / 1000.0
    println("Training time: " + totalTime + " seconds")
    (new Model(learnedParameters, normalFlipConstructor), totalTime)
  }

  def experiment(stream: BufferedWriter, testSet: List[Datum], scale: Int, trainingSet: List[Datum], truePredictiveAccuracy: Double) {
    val numEMIterations = scale
    val numSamplesPerTrainingExample = mHScaleFactor * scale

    def learner(parameters: Parameters): Algorithm = {
      parameters match {
        case ps: LearnableParameters => EMWithVE(numEMIterations, ps.b1, ps.b2, ps.b3, ps.b4, ps.b5, ps.b6, ps.b7, ps.b8, ps.b9)(parameters.universe)
        case _ => throw new IllegalArgumentException("Not learnable parameters")
      }
    }

    def parameterGetter(algorithm: Algorithm, parameter: Element[Double]): Double = {
      parameter match {
        case p: Parameter[Double] => p.expectedValue
        case _ => throw new IllegalArgumentException("Not a learnable parameter")
      }
    }

    def mh(ps: Parameters) =
      MetropolisHastings(trainingSetSize * numSamplesPerTrainingExample, ProposalScheme.default(ps.universe),
        ps.b1, ps.b2, ps.b3, ps.b4, ps.b5, ps.b6, ps.b7, ps.b8, ps.b9)(ps.universe)

    def probQueryGetter(algorithm: Algorithm, parameter: Element[Double]): Double = {
      algorithm match {
        case a: ProbQueryAlgorithm => a.expectation(parameter, (d: Double) => d)
        case _ => throw new IllegalArgumentException("Not a probability of query algorithm")
      }
    }

    println(trainingSetSize.toString + " training examples, scale = " + scale)

    println("Learning:")
    val (learnedModel, learningTime) = train(trainingSet, new LearnableParameters(new Universe), learner, parameterGetter, learningFlipConstructor)
    val (learnedParamErr, learnedPredAcc) = assessModel(learnedModel, testSet)

    println("Metropolis-Hastings:")
    val (mHModel, mHTime) = train(trainingSet, new BayesianParameters(new Universe), mh, probQueryGetter, normalFlipConstructor)
    val (mHParamErr, mHPredAcc) = assessModel(mHModel, testSet)

    stream.write(scale.toString + ", " + learningTime + ", " + mHTime + ", " + learnedParamErr + ", " + mHParamErr + ", " + truePredictiveAccuracy + ", " + learnedPredAcc + ", " + mHPredAcc + "\n")
    stream.flush()
  }

  def main(args: Array[String]) {
    val time0 = System.currentTimeMillis()
    val testSet = List.fill(testSetSize)(generateDatum())
    val trainingSet = List.fill(trainingSetSize)(generateDatum())
    val file = new File(filename)
    val outputStream = new BufferedWriter(new FileWriter(file))
    outputStream.write("Scale, Learning Time, MH Time, Learning Parameter Error, MCMC Parameter Error, True Prediction Accuracy, Learning Prediction Accuracy, MCMC Prediction Accuracy\n")
    outputStream.flush()
    println("True model:")
    val (trueParamErr, truePredAcc) = assessModel(new TrueModel, testSet)
    for { scale <- minScale to maxScale by scaleStep } {
      experiment(outputStream, testSet, scale, trainingSet, truePredAcc)
    }
    outputStream.close()
    val time1 = System.currentTimeMillis()
    println("Done: total time = " + ((time1 - time0) / 1000.0))
  }
}
