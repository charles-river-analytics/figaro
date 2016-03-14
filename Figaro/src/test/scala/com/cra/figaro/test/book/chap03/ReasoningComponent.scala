/*
 * ReasoningComponent.scala 
 * Book example unit test.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com), Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Feb 26, 2016
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.book.chap03

import scala.io.Source
import java.io.File
import com.cra.figaro.language.{Constant, Element, Universe}
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

// MXR (10-MAR-2016): some println statements commented out to minimize testing output
object ReasoningComponent {
  def loadResults(fileName: String) = {
    val source = Source.fromFile(fileName)
    val lines = source.getLines().toList
    val (numEmailsLine :: spamLine :: hasManyUnusualWordsGivenSpamLine :: hasManyUnusualWordsGivenNormalLine ::
        unusualWordGivenManyLine :: unusualWordGivenFewLine :: numWordsLine :: numFeatureWordsLine :: rest) = lines
    val numEmails = numEmailsLine.toInt
    val spamProbability = spamLine.toDouble
    val hasUnusualWordsGivenSpamProbability = hasManyUnusualWordsGivenSpamLine.toDouble
    val hasUnusualWordsGivenNormalProbability = hasManyUnusualWordsGivenNormalLine.toDouble
    val unusualWordGivenHasUnusualProbability = unusualWordGivenManyLine.toDouble
    val unusualWordGivenNotHasUnusualProbability = unusualWordGivenFewLine.toDouble
    val numWords = numWordsLine.toInt
    val numFeatureWords = numFeatureWordsLine.toInt

    var linesRemaining = rest
    var wordsGivenSpamProbabilities = Map[String, Double]()
    var wordsGivenNormalProbabilities = Map[String, Double]()
    var wordsAndCounts = List[(String, Int)]()

    for { i <- 0 until numWords } {
      val word :: countLine :: rest = linesRemaining
      linesRemaining = rest
      wordsAndCounts ::= (word, countLine.toInt)
    }

    for { i <- 0 until numFeatureWords } {
      val word :: givenSpamLine :: givenNormalLine :: rest = linesRemaining
      linesRemaining = rest
      wordsGivenSpamProbabilities += word -> givenSpamLine.toDouble
      wordsGivenNormalProbabilities += word -> givenNormalLine.toDouble
    }

    val dictionary = new Dictionary(numEmails)
    for {
      (word, count) <- wordsAndCounts
      i <- 0 until count
    } {
      dictionary.addWord(word)
    }

    val params = new LearnedParameters(
      spamProbability,
      hasUnusualWordsGivenSpamProbability,
      hasUnusualWordsGivenNormalProbability,
      unusualWordGivenHasUnusualProbability,
      unusualWordGivenNotHasUnusualProbability,
      wordsGivenSpamProbabilities,
      wordsGivenNormalProbabilities
    )
    (dictionary, params)
  }

  def classify(dictionary: Dictionary, parameters: LearnedParameters, fileName: String) = {
    val file = new File(fileName)
    val email = new Email(file)
    val model = new ReasoningModel(dictionary, parameters)
    email.observeEvidence(model, None, false)
    val algorithm = VariableElimination(model.isSpam)
    algorithm.start()
    val isSpamProbability = algorithm.probability(model.isSpam, true)
    // println("Spam probability: " + isSpamProbability)
    algorithm.kill()
    isSpamProbability
  }

  def main(args: Array[String]) {
    val emailFileName = "src/test/resources/BookData/Test/TestEmail_9.txt"
    val learningFileName = "src/test/resources/BookData/LearnedModel.txt"
    val (dictionary, parameters) = loadResults(learningFileName)
    classify(dictionary, parameters, emailFileName)
    println("Done!")
  }
}

class ReasoningComponentTest extends WordSpec with Matchers {
  Universe.createNew()
  val emailFileName = "src/test/resources/BookData/Test/TestEmail_9.txt"
  val learningFileName = "src/test/resources/BookData/LearnedModel.txt"
  
  "Reasoning Component" should {
    "produce a spam probability of 0.999999999632511" taggedAs (BookExample) in {
      val (dictionary, parameters) = ReasoningComponent.loadResults(learningFileName)
      val isSpamProbability = ReasoningComponent.classify(dictionary, parameters, emailFileName)
      isSpamProbability should be(0.999999999632511)
    }
  }
}
