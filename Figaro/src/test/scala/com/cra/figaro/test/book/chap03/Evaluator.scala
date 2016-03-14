/*
 * Evaluator.scala 
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

import com.cra.figaro.language.Universe
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object Evaluator {
  def main(args: Array[String]) = {
    val testDirectoryName = "src/test/resources/BookData/Test"
    val labelFileName = "src/test/resources/BookData/Labels.txt"
    val kbFileName = "src/test/resources/BookData/LearnedModel.txt"
    val threshold = 0.5

    val emails = LearningComponent.readEmails(testDirectoryName)
    val labels = LearningComponent.readLabels(labelFileName)
    val (dictionary, learningResults) = ReasoningComponent.loadResults(kbFileName)

    var truePositives = 0
    var falseNegatives = 0
    var falsePositives = 0
    var trueNegatives = 0

    for { (fileName, email) <- emails } {
      println(fileName)
      Universe.createNew()
      val isSpamProbability = ReasoningComponent.classify(dictionary, learningResults, testDirectoryName + "/" + fileName)
      val prediction = isSpamProbability >= threshold
      (labels.get(fileName), prediction) match {
        case (Some(true), true) => truePositives += 1
        case (Some(true), false) => falseNegatives += 1
        case (Some(false), true) => falsePositives += 1
        case (Some(false), false) => trueNegatives += 1
        case _ => ()
      }
    }

    val accuracy = (truePositives + trueNegatives).toDouble / (truePositives + falseNegatives + falsePositives + trueNegatives)
    val precision = truePositives.toDouble / (truePositives + falsePositives)
    val recall = truePositives.toDouble / (truePositives + falseNegatives)

    println("True positives: " + truePositives)
    println("False negatives: " + falseNegatives)
    println("False positives: " + falsePositives)
    println("True negatives: " + trueNegatives)
    println("Threshold: " + threshold)
    println("Accuracy: " + accuracy)
    println("Precision: " + precision)
    println("Recall: " + recall)
  }
}

class EvaluatorTest extends WordSpec with Matchers {
  Universe.createNew()
  val testDirectoryName = "src/test/resources/BookData/Test"
  val labelFileName = "src/test/resources/BookData/Labels.txt"
  val kbFileName = "src/test/resources/BookData/LearnedModel.txt"
  val threshold = 0.5
  
  val emails = LearningComponent.readEmails(testDirectoryName)
  val labels = LearningComponent.readLabels(labelFileName)
  val (dictionary, learningResults) = ReasoningComponent.loadResults(kbFileName)
  
  var truePositives = 0
  var falseNegatives = 0
  var falsePositives = 0
  var trueNegatives = 0
  
  for { (fileName, email) <- emails } {
    Universe.createNew()
    val isSpamProbability = ReasoningComponent.classify(dictionary, learningResults, testDirectoryName + "/" + fileName)
    val prediction = isSpamProbability >= threshold
    (labels.get(fileName), prediction) match {
      case (Some(true), true) => truePositives += 1
      case (Some(true), false) => falseNegatives += 1
      case (Some(false), true) => falsePositives += 1
      case (Some(false), false) => trueNegatives += 1
      case _ => ()
    }
  }
  val accuracy = (truePositives + trueNegatives).toDouble / (truePositives + falseNegatives + falsePositives + trueNegatives)
  val precision = truePositives.toDouble / (truePositives + falsePositives)
  val recall = truePositives.toDouble / (truePositives + falseNegatives)

  "Evaluator" should {
    "have true positives equal to 39" taggedAs (BookExample) in {
      truePositives should be(39)
    }
    "have false negatives equal to 0" taggedAs (BookExample) in {
      falseNegatives should be(0)
    }
    "have false positives equal to 0" taggedAs (BookExample) in {    
      falsePositives should be(0)
    }
    "have true negatives equal to 61" taggedAs (BookExample) in {
      trueNegatives should be(61)
    }
    "have an accuracy equal to 1.0" taggedAs (BookExample) in {    
      accuracy should be(1.0)
    }
    "have a precision equal to 1.0" taggedAs (BookExample) in {    
      precision should be(1.0)
    }
    "have a recall equal to 1.0" taggedAs (BookExample) in {    
      recall should be(1.0)
    }
  }
}
