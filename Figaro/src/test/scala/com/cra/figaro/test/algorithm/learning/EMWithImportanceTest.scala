/*
 * EMWithImportanceTest.scala
 * Tests for the EM algorithm
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 6, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.learning

import org.scalatest.Matchers
import org.scalatest.{ PrivateMethodTester, WordSpec }
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.learning._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.compound._
import com.cra.figaro.language._
import com.cra.figaro.language.Universe._
import com.cra.figaro.util._
import com.cra.figaro.util.random
import scala.math.abs
import java.io._
import com.cra.figaro.test.tags.NonDeterministic

class EMWithImportanceTest extends WordSpec with PrivateMethodTester with Matchers {
  "Expectation Maximization with importance sampling" when
    {
      "used to estimate a Beta parameter" should
        {
          "detect bias after a large enough number of trials" in
            {
              val universe = Universe.createNew
              val b = BetaParameter(2, 2)

              for (i <- 1 to 7) {

                val f = Flip(b)
                f.observe(true)
              }

              for (i <- 1 to 3) {

               val f = Flip(b)
                f.observe(false)
              }

              val algorithm = EMWithImportance(2, 100, b)(universe)
              algorithm.start

              val result = b.getLearnedElement
              algorithm.kill
              result.prob should be(0.6666 +- 0.01)

            }

          "take the prior concentration parameters into account" in
            {
              val universe = Universe.createNew
              val b = BetaParameter(3.0, 7.0)

              for (i <- 1 to 7) {

                val f = Flip(b)
                f.observe(true)
              }

              for (i <- 1 to 3) {

                val f = Flip(b)
                f.observe(false)
              }

              val algorithm = EMWithImportance(2, 100, b)(universe)
              algorithm.start

              val result = b.getLearnedElement
              algorithm.kill
              result.prob should be(0.50 +- 0.01)

            }


          "learn the bias from observations of binomial elements" in {
              val universe = Universe.createNew
              val b = BetaParameter(2, 2)

              val b1 = Binomial(7, b)
              b1.observe(6)
              val b2 = Binomial(3, b)
              b2.observe(1)

              val algorithm = EMWithImportance(2, 100, b)(universe)
              algorithm.start

              val result = b.getLearnedElement
              algorithm.kill
              result.prob should be(0.6666 +- 0.01)

            
          }
        }

          "correctly use a uniform prior" in {
              val universe = Universe.createNew
              val b = BetaParameter(1, 1)

              val b1 = Binomial(7, b)
              b1.observe(6)
              val b2 = Binomial(3, b)
              b2.observe(1)

              val algorithm = EMWithImportance(2, 100, b)(universe)
              algorithm.start

              val result = b.getLearnedElement
              algorithm.kill
              result.prob should be(0.7 +- 0.01)

            
          }

          "used to estimate a Dirichlet parameter with two concentration parameters" should
        {

          "detect bias after a large enough number of trials" in
            {
              val universe = Universe.createNew
              val b = DirichletParameter(2, 2)

              for (i <- 1 to 7) {

                val f = Select(b, true, false)
                f.observe(true)
              }

              for (i <- 1 to 3) {

                val f = Select(b, true, false)
                f.observe(false)
              }

              val algorithm = EMWithImportance(2, 1000, b)(universe)
              algorithm.start

              val result = b.getLearnedElement(List(true, false))
              algorithm.kill
              result.probs(0) should be(0.6666 +- 0.01)

            }

          "take the prior concentration parameters into account" in
            {
              val universe = Universe.createNew

              val b = DirichletParameter(3, 7)

              for (i <- 1 to 7) {

                val f = Select(b, true, false)
                f.observe(true)
              }

              for (i <- 1 to 3) {

                val f = Select(b, true, false)
                f.observe(false)
              }

              val algorithm = EMWithImportance(2, 1000, b)(universe)
              algorithm.start

              val result = b.getLearnedElement(List(true, false))
              algorithm.kill
              result.probs(0) should be(0.50 +- 0.01)

            }

        }

      "used to estimate a Dirichlet parameter with three concentration parameters" should
        {

          "calculate sufficient statistics in the correct order for long lists of concentration parameters" in
            {
              val universe = Universe.createNew
              val alphas = Seq[Double](0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476, 0.0476)
              val d = DirichletParameter(alphas: _*)
              val outcomes = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
              val outcome = Select(d, outcomes: _*)
              val algorithm = EMWithImportance(2, 1000, d)
              algorithm.start

              val result = d.getLearnedElement(outcomes)
              algorithm.kill
              result.probs(0) should be(0.04 +- 0.01)
              result.probs(1) should be(0.04 +- 0.01)
              result.probs(2) should be(0.04 +- 0.01)
              result.probs(3) should be(0.04 +- 0.01)
              result.probs(4) should be(0.04 +- 0.01)
              result.probs(5) should be(0.04 +- 0.01)
              result.probs(6) should be(0.04 +- 0.01)
              result.probs(7) should be(0.04 +- 0.01)
              result.probs(8) should be(0.04 +- 0.01)
              result.probs(9) should be(0.04 +- 0.01)
              result.probs(10) should be(0.04 +- 0.01)
              result.probs(11) should be(0.04 +- 0.01)
              result.probs(12) should be(0.04 +- 0.01)
              result.probs(13) should be(0.04 +- 0.01)
              result.probs(14) should be(0.04 +- 0.01)
              result.probs(15) should be(0.04 +- 0.01)
              result.probs(16) should be(0.04 +- 0.01)
              result.probs(17) should be(0.04 +- 0.01)
              result.probs(18) should be(0.04 +- 0.01)
              result.probs(19) should be(0.04 +- 0.01)
              result.probs(20) should be(0.04 +- 0.01)
              result.probs(21) should be(0.04 +- 0.01)

            }

          "calculate sufficient statistics in the correct order for long lists of concentration parameters, taking into account a condition" taggedAs(NonDeterministic) in
            {
              val universe = Universe.createNew
              val alphas = Seq[Double](1.0476, 1.0476, 1.0476, 1.0476, 1.0476)
              val d = DirichletParameter(alphas: _*)
              val outcomes = List(2, 3, 4, 5, 6)

              for (i <- 1 to 10) {
                val outcome = Select(d, outcomes: _*)
                outcome.addCondition(x => x >= 3 && x <= 6)
              }

              val algorithm = EMWithImportance(2, 1000, d)
              algorithm.start
              val result = d.getLearnedElement(outcomes)
              algorithm.kill
              result.probs(0) should be(0.0 +- 0.01)
              result.probs(1) should be(0.25 +- 0.01)
              result.probs(2) should be(0.25 +- 0.01)
              result.probs(3) should be(0.25 +- 0.01)
              result.probs(4) should be(0.25 +- 0.01)
            }

          "detect bias after a large enough number of trials" in
            {
              val universe = Universe.createNew
              val b = DirichletParameter(2, 2, 2)
              val outcomes = List(1, 2, 3)
              val errorTolerance = 0.01
              for (i <- 1 to 8) {

               val f = Select(b, outcomes: _*)
               f.observe(1)

              }

              for (i <- 1 to 6) {
                val f = Select(b, outcomes: _*)
                f.observe(2)
              }

              for (i <- 1 to 2) {
                val f = Select(b, outcomes: _*)
                f.observe(3)
              }

              val algorithm = EMWithImportance(2, 1000, b)(universe)
              algorithm.start

              val result = b.getLearnedElement(outcomes)
              algorithm.kill

              //9/19
              result.probs(0) should be(0.47 +- errorTolerance)

              //7/19
              result.probs(1) should be(0.36 +- errorTolerance)
              //3/19
              result.probs(2) should be(0.15 +- errorTolerance)

            }

          "take the prior concentration parameters into account" in
            {
              val universe = Universe.createNew
              val b = DirichletParameter(2.0, 3.0, 2.0)
              val outcomes = List(1, 2, 3)

              for (i <- 1 to 3) {

                val f2 = Select(b, outcomes: _*)
                f2.observe(1)
              }

              for (i <- 1 to 2) {
                val f3 = Select(b, outcomes: _*)
                f3.observe(2)
              }

              for (i <- 1 to 3) {

                val f1 = Select(b, outcomes: _*)
                f1.observe(3)
              }

              val algorithm = EMWithImportance(2, 1000, b)(universe)
              algorithm.start

              val result = b.getLearnedElement(outcomes)
              algorithm.kill
              result.probs(0) should be(0.33 +- 0.01)
              result.probs(1) should be(0.33 +- 0.01)
              result.probs(2) should be(0.33 +- 0.01)

            }

          "correctly use a uniform prior" in 
            {
              val universe = Universe.createNew
              val b = DirichletParameter(1.0, 1.0, 1.0)
              val outcomes = List(1, 2, 3)

              for (i <- 1 to 3) {

                val f2 = Select(b, outcomes: _*)
                f2.observe(1)
              }

              for (i <- 1 to 3) {
                val f3 = Select(b, outcomes: _*)
                f3.observe(2)
              }

              for (i <- 1 to 3) {
                val f1 = Select(b, outcomes: _*)
                f1.observe(3)
              }
        
              val algorithm = EMWithImportance(2, 1000, b)(universe)
              algorithm.start

              val result = b.getLearnedElement(outcomes)
              algorithm.kill
              result.probs(0) should be(0.33 +- 0.01)
              result.probs(1) should be(0.33 +- 0.01)
              result.probs(2) should be(0.33 +- 0.01)
            }
        }

          "used to estimate multiple parameters" should
            {

              "leave parameters having no observations unchanged" in
                {
                  val universe = Universe.createNew
                  val d = DirichletParameter(2.0, 4.0, 2.0)
                  val b = BetaParameter(2.0, 2.0)
                  val outcomes = List(1, 2, 3)

                  for (i <- 1 to 4) {

                    val f2 = Select(d, outcomes: _*)
                    f2.observe(1)
                  }

                  for (i <- 1 to 2) {
                    val f3 = Select(d, outcomes: _*)
                    f3.observe(2)
                  }

                 for (i <- 1 to 4) {
                    val f1 = Select(d, outcomes: _*)
                    f1.observe(3)
                  }

                  val algorithm = EMWithImportance(2, 1000, d, b)(universe)
                  algorithm.start

                  val result = d.getLearnedElement(outcomes)
                  algorithm.kill
                  result.probs(0) should be(0.33 +- 0.01)
                  result.probs(1) should be(0.33 +- 0.01)
                  result.probs(2) should be(0.33 +- 0.01)

                  val betaResult = b.getLearnedElement
                  betaResult.prob should be(0.5)

                }

              "correctly estimate all parameters with observations" in
                {
                  val universe = Universe.createNew
                  val d = DirichletParameter(2.0, 3.0, 2.0)
                  val b = BetaParameter(3.0, 7.0)
                  val outcomes = List(1, 2, 3)

                  for (i <- 1 to 3) {

                    val f2 = Select(d, outcomes: _*)
                    f2.observe(1)
                  }

                  for (i <- 1 to 2) {
                    val f3 = Select(d, outcomes: _*)
                    f3.observe(2)
                  }

                  for (i <- 1 to 3) {
                    val f1 = Select(d, outcomes: _*)
                    f1.observe(3)
                  }

                  for (i <- 1 to 7) {
                    val f = Flip(b)
                    f.observe(true)
                  }

                  for (i <- 1 to 3) {
                    val f = Flip(b)

                   f.observe(false)
                  }

                  val algorithm = EMWithImportance(2, 1000, b,d)(universe)
                  algorithm.start

                  val result = d.getLearnedElement(outcomes)

                  result.probs(0) should be(0.33 +- 0.01)
                  result.probs(1) should be(0.33 +- 0.01)
                  result.probs(2) should be(0.33 +- 0.01)

                  val betaResult = b.getLearnedElement
                  betaResult.prob should be(0.5 +- 0.01)

                }
          }

      val observationProbability = 0.7
      val trainingSetSize = 100
      val testSetSize = 100
      val minScale = 10
      val maxScale = 10
      val scaleStep = 2

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
      object TrueModel extends Model(TrueParameters, normalFlipConstructor)

      case class Datum(x: Boolean, y: Boolean, z: Boolean, w: Boolean)

      def generateDatum(): Datum = {
        val model = TrueModel
        model.universe.generateAll()
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
            val alg = VariableElimination(model.x)(model.universe)
            alg.start()
            alg.probability(model.x, datum.x)
          case 1 =>
            model.x.observe(datum.x)
            model.z.observe(datum.z)
            model.w.observe(datum.w)
            val alg = VariableElimination(model.y)(model.universe)
            alg.start()
            alg.probability(model.y, datum.y)
          case 2 =>
            model.x.observe(datum.x)
            model.y.observe(datum.y)
            model.w.observe(datum.w)
            val alg = VariableElimination(model.z)(model.universe)
            alg.start()
            alg.probability(model.z, datum.z)
          case 3 =>
            model.x.observe(datum.x)
            model.y.observe(datum.y)
            model.z.observe(datum.z)
            val alg = VariableElimination(model.w)(model.universe)
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
        def extractParameter(parameter: Element[Double], name: String) = 
          {
           parameter match
           {
             case b: AtomicBeta =>
               {

                 Constant(valueGetter(algorithm, parameter))(name, resultUniverse)
               }
             case _ => Constant(valueGetter(algorithm, parameter))(name, resultUniverse)
           }
           
          }
        
        val learnedParameters = new Parameters(resultUniverse) {
          val b1 = extractParameter(parameters.b1, "b1"); b1.generate()
          val b2 = extractParameter(parameters.b2, "b2"); b2.generate()
          val b3 = extractParameter(parameters.b3, "b3"); b3.generate()
          val b4 = extractParameter(parameters.b4, "b4"); b4.generate()
          val b5 = extractParameter(parameters.b5, "b5"); b5.generate()
          val b6 = extractParameter(parameters.b6, "b6"); b6.generate()
          val b7 = extractParameter(parameters.b7, "b7"); b7.generate()
          val b8 = extractParameter(parameters.b8, "b8"); b8.generate()
          val b9 = extractParameter(parameters.b9, "b9"); b9.generate()
        }

        algorithm.kill()
        val time1 = System.currentTimeMillis()
        val totalTime = (time1 - time0) / 1000.0
        println("Training time: " + totalTime + " seconds")
        (new Model(learnedParameters, normalFlipConstructor), totalTime)
      }

      "derive parameters within a reasonable accuracy for random data" taggedAs(NonDeterministic) in
        {

          val numEMIterations = 5
          val testSet = List.fill(testSetSize)(generateDatum())
          val trainingSet = List.fill(trainingSetSize)(generateDatum())

          def learner(parameters: Parameters): Algorithm = {
            parameters match {
              case ps: LearnableParameters => EMWithImportance(numEMIterations, 1000, ps.b1, ps.b2, ps.b3, ps.b4, ps.b5, ps.b6, ps.b7, ps.b8, ps.b9)(parameters.universe)
              case _ => throw new IllegalArgumentException("Not learnable parameters")
            }
          }

          def parameterGetter(algorithm: Algorithm, parameter: Element[Double]): Double = {
            parameter match {
              case p: Parameter[Double] => {
                p.MAPValue
              }
              case _ => throw new IllegalArgumentException("Not a learnable parameter")
            }
          }
          val (trueParamErr, truePredAcc) = assessModel(TrueModel, testSet)
          val (learnedModel, learningTime) = train(trainingSet, new LearnableParameters(new Universe), learner, parameterGetter, learningFlipConstructor)
          val (learnedParamErr, learnedPredAcc) = assessModel(learnedModel, testSet)

          println(learnedParamErr)
          println(learnedPredAcc)
          learnedParamErr should be(0.00 +- 0.12)
          learnedPredAcc should be(truePredAcc +- 0.12)

        }

    }

}