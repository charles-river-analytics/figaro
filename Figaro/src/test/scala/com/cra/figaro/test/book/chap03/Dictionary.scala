/*
 * Dictionary.scala 
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
import scala.collection.mutable.Map
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

class Dictionary(initNumEmails: Int) {
  val counts: Map[String, Int] = Map()
  var numEmails = initNumEmails

  def addWord(word: String) {
    counts += word -> (getCount(word) + 1)
  }

  def addEmail(email: Email) {
    numEmails += 1
    for { word <- email.allWords } {
      addWord(word)
    }
  }

  object OrderByCount extends Ordering[String] {
    def compare(a: String, b: String) = getCount(b) - getCount(a)
  }
  def words = counts.keySet.toList.sorted(OrderByCount)

  def nonStopWords = words.dropWhile(counts(_) >= numEmails * Dictionary.stopWordFraction)
  def featureWords = nonStopWords.take(Dictionary.numFeatures)

  def getCount(word: String) =
    counts.getOrElse(word, 0)

  def isUnusual(word: String, learning: Boolean) =
    if (learning) getCount(word) <= 1
    else getCount(word) <= 0
}

object Dictionary {
  def fromEmails(emails: Traversable[Email]) = {
    val result = new Dictionary(0)
    for { email <- emails } { result.addEmail(email) }
    result
  }

  val stopWordFraction = 0.15
  val numFeatures = 100

  def main(args: Array[String]) = {
    val emails = LearningComponent.readEmails("src/test/resources/BookData/Test")
    val dict = Dictionary.fromEmails(emails.map(_._2))
    println("Total number of words: " + dict.words.length)
    println("Number of feature words: " + dict.featureWords.length)
    println("\nAll words and counts:\n")
    println(dict.words.map(word => word + " " + dict.getCount(word)).mkString("\n"))
    println("\nFeature words and counts:\n")
    println("Feature words:\n")
    println(dict.featureWords.map(word => word + " " + dict.getCount(word)).mkString("\n"))
  }
}

class DictionaryTest extends WordSpec with Matchers {
  Universe.createNew()
  val emails = LearningComponent.readEmails("src/test/resources/BookData/Test")
  val dict = Dictionary.fromEmails(emails.map(_._2))

  "Dictionary" should {
    "have a total number of words equal to 12922" taggedAs (BookExample) in {
      dict.words.length should be(12922)
    }
    "have a number of feature words equal to 100" taggedAs (BookExample) in {
      dict.featureWords.length should be(100)
    }
  }
}
