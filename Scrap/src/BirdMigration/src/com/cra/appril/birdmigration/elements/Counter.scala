package com.cra.appril.birdmigration.elements

//This can be in com.cra.appril
//or even in figaro
//nothing about it is really specific to bird migration.
import scala.collection.mutable.ListBuffer
import scala.collection._
import com.cra.figaro.language.Apply
import com.cra.figaro.language.Element
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Apply
import com.cra.figaro.language.Element
import com.cra.figaro.language.Constant
import com.cra.figaro.algorithm.Abstraction
import com.cra.figaro.language.Universe

object Counter {

  def makeBoolCounter(inputs: List[Element[Boolean]]): Element[Int] = {
    val initial = Constant(0)
    val increment = (b: Boolean, i: Int) => if (b) i + 1 else i
    if (inputs.isEmpty) {
      initial
    } else {
      var counter = Apply(inputs(0), initial, increment)

      if (inputs.size == 1) {
        counter
      } else {
        var index = 1
        var previous: Element[Int] = counter

        while (index < inputs.size) {
          counter = Apply(inputs(index), previous, increment)
          previous = counter
          index += 1
        }

        counter
      }
    }
  }

  def makeIntCounterWithCeiling(inputs: List[Element[Int]], ceiling: Int): Element[Int] = {
    //println("Counting birds in cell")
    val initial = Constant(0)
    val increment = (i1: Int, i2: Int) => if (i1 + i2 < ceiling) i1 + i2 else ceiling
    if (inputs.isEmpty) {
      println("returning empty")
      return initial
    } else {
      var counter = Apply(inputs(0), initial, increment)("counter" + 0, Universe.universe)

      if (inputs.size == 1) {
        println("returning 1")
        return counter
      } else {

        var index = 1
        var previous: Element[Int] = counter

        while (index < inputs.size) {
          counter = Apply(inputs(index), previous, increment)("counter" + index, Universe.universe)
          previous = counter
          index += 1
        }
        println("returning counter")
        return counter
      }
    }
  }
  
  def makeIntCounter(inputs: List[Element[Int]]): Element[Int] = {
    if (inputs.isEmpty) {
      //println("returning empty")
      return  Constant(0)
    } else {
      var counter : Element[Int]= Apply(inputs(0), (i1: Int) => i1)

      if (inputs.size == 1) {
        //println("returning 1")
        return counter
      } else {

        var index = 1
        var previous: Element[Int] = counter

        while (index < inputs.size) {
          counter = Apply(inputs(index), previous, (i1: Int, i2: Int) => i1 + i2)
          previous = counter
          index += 1
        }
       // println("returning counter")
        return counter
      }
    }
  }

  def apply(input: List[Element[Int]]) = makeIntCounter(input)
  def apply(input: List[Element[Int]], ceiling: Int) = makeIntCounterWithCeiling(input,ceiling)
  //def apply(input: List[Element[List[Int]]], target: Int) = makeIntCounterWithTarget(input, target)
}

