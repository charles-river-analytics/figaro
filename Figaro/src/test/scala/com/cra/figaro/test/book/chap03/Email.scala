/*
 * Email.scala 
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
import java.io.File
import scala.io.Source
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

class Email(file: File) {
  def getAllWords() = {
    def getWords(line: String): List[String] = {
      for {
        rawWord <- line.split(Array(' ', '\t', '\n')).toList
        word = rawWord.filter((c: Char) => c.isLetterOrDigit)
        if !word.isEmpty
      } yield word.toLowerCase()
    }

    val source = Source.fromFile(file)("ISO-8859-1")
    val allLines = source.getLines().toList

    val allWordsWithRepeats =
      for {
        line <- allLines
        word <- getWords(line)
      } yield word

    allWordsWithRepeats.toSet
  }

  val allWords: Set[String] = getAllWords()

  def observeEvidence(model: Model, label: Option[Boolean], learning: Boolean) {
    label match {
      case Some(b) => model.isSpam.observe(b)
      case None => ()
    }

    for {
      (word, element) <- model.hasWordElements
    } {
      element.observe(allWords.contains(word))
    }

    val obsNumUnusualWords =
      allWords.filter((word: String) => model.dictionary.isUnusual(word, learning)).size
    val unusualWordFraction = obsNumUnusualWords * Model.binomialNumTrials / allWords.size
    model.numUnusualWords.observe(unusualWordFraction)
  }
}

object Email {
  def main(args: Array[String]) {
    val email = new Email(new File("src/test/resources/BookData/Test/TestEmail_9.txt"))
    println(email.allWords)
  }
}

class EmailTest extends WordSpec with Matchers {
  Universe.createNew()
  val email = new Email(new File("src/test/resources/BookData/Test/TestEmail_9.txt"))

  "Email" should {
    "have a total number of words equal to 358" taggedAs (BookExample) in {
      email.allWords.size should be(358)
    }
  }
}