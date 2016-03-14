/*
 * HelloWorld.scala 
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

package com.cra.figaro.test.book.chap01

import com.cra.figaro.language.Universe
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.language.{Flip, Select}			//#A
import com.cra.figaro.library.compound.If				//#A
import com.cra.figaro.algorithm.factored.VariableElimination	//#A

object HelloWorldFigaro {
  val sunnyToday = Flip(0.2)					//#B
  val greetingToday = If(sunnyToday, 				//#C
       Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),	//#C
       Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again"))	//#C
  val sunnyTomorrow = If(sunnyToday, Flip(0.8), Flip(0.05))	//#D
  val greetingTomorrow = If(sunnyTomorrow,				//#E
       Select(0.5 -> "Hello, world!", 0.5 -> "Howdy, universe!"),	//#E
       Select(0.1 -> "Hello, world!", 0.9 -> "Oh no, not again"))	//#E
  
  def predict() : Double = {
    val result = VariableElimination.probability(greetingToday, "Hello, world!") // #F
    println("Today's greeting is \"Hello, world!\" " +
            "with probability " + result + ".")			//#G
    result
  }

  def infer() : Double = {
    greetingToday.observe("Hello, world!")				//#H
    val result = VariableElimination.probability(sunnyToday, true)		//#F
    println("If today's greeting is \"Hello, world!\", today's " +
            "weather is sunny with probability " + result + ".")	//#G
    result
  }

  def learnAndPredict() : Double = {
    val result = 
      VariableElimination.probability(greetingTomorrow, "Hello, world!")	//#F
    println("If today's greeting is \"Hello, world!\", " +		                               
            "tomorrow's greeting will be \"Hello, world!\" " +
            "with probability " + result + ".")			//#G
    result
  }
  
  def main(args: Array[String]) {					//#I
    predict()							//#I
    infer()								//#I
    learnAndPredict()						//#I
  }
}

class HelloWorldTest extends WordSpec with Matchers {
  Universe.createNew()
  "Chap01 Variable Elimination value" should {
    "equal 0.27999999999999997 when predicted" taggedAs (BookExample) in {
        HelloWorldFigaro.predict() should be(0.27999999999999997)							//#I
    }

    "equal 0.4285714285714285 when inferred" taggedAs (BookExample) in {
        HelloWorldFigaro.infer() should be(0.4285714285714285) 							//#I
    }

    "equal 0.24857142857142858 when learned and predicted" taggedAs (BookExample) in {
        HelloWorldFigaro.learnAndPredict() should be(0.24857142857142858)						//#I
    }
  }
}