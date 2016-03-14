/*
 * Model.scala 
 * Book example unit test support file.
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

import com.cra.figaro.language.{Element, Constant, Flip, Universe}
import com.cra.figaro.library.compound.If
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import scala.collection.Map

class PriorParameters(dictionary: Dictionary) {
    val spamProbability = Beta(2,3)
    val wordGivenSpamProbabilities = dictionary.featureWords.map(word => (word, Beta(2,2)))
    val wordGivenNormalProbabilities = dictionary.featureWords.map(word => (word, Beta(2,2)))
    val hasManyUnusualWordsGivenSpamProbability = Beta(2,2)
    val hasManyUnusualWordsGivenNormalProbability = Beta(2, 21)
    val unusualWordGivenManyProbability = Beta(2,2)
    val unusualWordGivenFewProbability = Beta(2,7)
    val fullParameterList =
      spamProbability ::
      hasManyUnusualWordsGivenSpamProbability ::
      hasManyUnusualWordsGivenNormalProbability ::
      unusualWordGivenManyProbability ::
      unusualWordGivenFewProbability ::
      wordGivenSpamProbabilities.map(pair => pair._2) :::
      wordGivenNormalProbabilities.map(pair => pair._2)
  }

class LearnedParameters(
  val spamProbability: Double,
  val hasManyUnusualWordsGivenSpamProbability: Double,
  val hasManyUnusualWordsGivenNormalProbability: Double,
  val unusualWordGivenManyProbability: Double,
  val unusualWordGivenFewProbability: Double,
  val wordGivenSpamProbabilities: Map[String, Double],
  val wordGivenNormalProbabilities: Map[String, Double]
)

abstract class Model(val dictionary: Dictionary) {
  val isSpam: Element[Boolean]

  val hasWordElements: List[(String, Element[Boolean])]

  val hasManyUnusualWords: Element[Boolean]

  val numUnusualWords: Element[Int]
}

class LearningModel(dictionary: Dictionary, parameters: PriorParameters) extends Model(dictionary) {
  val isSpam = Flip(parameters.spamProbability)

  val hasWordElements = {
    val wordGivenSpamMap = Map(parameters.wordGivenSpamProbabilities:_*)
    val wordGivenNormalMap = Map(parameters.wordGivenNormalProbabilities:_*)
    for { word <- dictionary.featureWords } yield {
      val givenSpamProbability = wordGivenSpamMap(word)
      val givenNormalProbability = wordGivenNormalMap(word)
      val hasWordIfSpam = Flip(givenSpamProbability)
      val hasWordIfNormal = Flip(givenNormalProbability)
      (word, If(isSpam, hasWordIfSpam, hasWordIfNormal))
    }
  }

  val hasManyUnusualIfSpam = Flip(parameters.hasManyUnusualWordsGivenSpamProbability)
  val hasManyUnusualIfNormal = Flip(parameters.hasManyUnusualWordsGivenNormalProbability)
  val hasManyUnusualWords = If(isSpam, hasManyUnusualIfSpam, hasManyUnusualIfNormal)

  val numUnusualIfHasMany = Binomial(Model.binomialNumTrials, parameters.unusualWordGivenManyProbability)
  val numUnusualIfHasFew = Binomial(Model.binomialNumTrials, parameters.unusualWordGivenFewProbability)
  val numUnusualWords = If(hasManyUnusualWords, numUnusualIfHasMany, numUnusualIfHasFew)
}

class ReasoningModel(dictionary: Dictionary, parameters: LearnedParameters) extends Model(dictionary) {
  val isSpam = Flip(parameters.spamProbability)

  val hasWordElements = {
    for { word <- dictionary.featureWords } yield {
      val givenSpamProbability = parameters.wordGivenSpamProbabilities(word)
      val givenNormalProbability = parameters.wordGivenNormalProbabilities(word)
      val hasWordIfSpam = Flip(givenSpamProbability)
      val hasWordIfNormal = Flip(givenNormalProbability)
      (word, If(isSpam, hasWordIfSpam, hasWordIfNormal))
    }
  }

  val hasManyUnusualIfSpam = Flip(parameters.hasManyUnusualWordsGivenSpamProbability)
  val hasManyUnusualIfNormal = Flip(parameters.hasManyUnusualWordsGivenNormalProbability)
  val hasManyUnusualWords = If(isSpam, hasManyUnusualIfSpam, hasManyUnusualIfNormal)

  val numUnusualIfHasMany = Binomial(Model.binomialNumTrials, parameters.unusualWordGivenManyProbability)
  val numUnusualIfHasFew = Binomial(Model.binomialNumTrials, parameters.unusualWordGivenFewProbability)
  val numUnusualWords = If(hasManyUnusualWords, numUnusualIfHasMany, numUnusualIfHasFew)
}

object Model {
  val binomialNumTrials = 20
}
