/*
 * MAPLearning.scala 
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

package com.cra.figaro.test.book.chap13

import com.cra.figaro.language.Universe
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.library.compound.If
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.patterns.learning.ModelParameters
import com.cra.figaro.patterns.learning.ParameterCollection
import com.cra.figaro.algorithm.learning.EMWithBP
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.algorithm.learning.ExpectationMaximization
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object MAPLearning {
  class Email(val text: List[String], val label: String)

  // In Chapter 3, the emails were drawn automatically from the training and test sets.
  // They are hardcoded here to keep the example simple.
  val trainingEmail1 = new Email(List("Hello, would you like to receive a free book?"), "spam")
  val trainingEmail2 = new Email(List("Please reply to my question by Tuesday!"), "normal")
  val trainingEmail3 = new Email(List("Hello, I have a question about your book."), "normal")
  val trainingEmails = List(trainingEmail1, trainingEmail2, trainingEmail3)

  // Illustrative list of feature words. In Chapter 3, the feature words were derived from the training emails.
  val featureWords = List("hello", "reply", "question", "free", "book")

  val params = ModelParameters()
  val spamProbability = Beta(2,3)("spam probability", params)
  val wordGivenSpamProbabilities =
    featureWords.map(word => (word, Beta(2,2)(word + " given spam", params))).toMap
  val wordGivenNormalProbabilities =
    featureWords.map(word => (word, Beta(2,2)(word + " given normal", params))).toMap

  class EmailModel(paramCollection: ParameterCollection) {
    val isSpam = Flip(paramCollection.get("spam probability"))

    val hasWordElements = {
      for { word <- featureWords } yield {
        val givenSpamProbability = paramCollection.get(word + " given spam")
        val givenNormalProbability = paramCollection.get(word + " given normal")
        val hasWord =
          If(isSpam, Flip(givenSpamProbability), Flip(givenNormalProbability))
        (word, hasWord)
      }
    }

    val hasWord = hasWordElements.toMap
  }

  for { email <- trainingEmails } {
    val model = new EmailModel(params.priorParameters)
    for { word <- featureWords } {
      model.hasWord(word).observe(email.text.contains(word))
    }
    model.isSpam.observe(email.label == "spam")
  }

  def main(args: Array[String]) {
    val learningAlg = EMWithVE(10, params)
    learningAlg.start()

    val futureEmail = new Email(List("Feel free to reply if you have any ideas."), "unknown")
    val futureModel = new EmailModel(params.posteriorParameters)
    for { word <- featureWords } {
      futureModel.hasWord(word).observe(futureEmail.text.contains(word))
    }
    
    learningAlg.stop()

    val result = VariableElimination.probability(futureModel.isSpam, true)
    println("Probability new email is spam = " + result)

    learningAlg.kill()  
  }
}

class MAPLearningTest extends WordSpec with Matchers {
  Universe.createNew()
  val learningAlg = EMWithVE(10, MAPLearning.params)
  learningAlg.start()

  val futureEmail = new MAPLearning.Email(List("Feel free to reply if you have any ideas."), "unknown")
  val futureModel = new MAPLearning.EmailModel(MAPLearning.params.posteriorParameters)
  for { word <- MAPLearning.featureWords } {
    futureModel.hasWord(word).observe(futureEmail.text.contains(word))
  }
  
  learningAlg.stop()

  val result = VariableElimination.probability(futureModel.isSpam, true)
  println("Probability new email is spam = " + result)
  
  learningAlg.kill()
  
  "MAP Learning" should {
    "produce a probability new email is spam = 0.2171438039742 +- 0.0000000000001" taggedAs (BookExample) in {
      result should be (0.2171438039742 +- 0.0000000000001)
    }
  }
}
