/*
 * Restaurant.scala 
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

package com.cra.figaro.test.book.chap08

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.discrete.{FromRange, Poisson}
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.sampling.Importance
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object Restaurant {
  val numSteps = 12
  val capacity = 10

  val seated: Array[Element[List[Int]]] = Array.fill(numSteps)(Constant(List()))
  val waiting: Array[Element[Int]] = Array.fill(numSteps)(Constant(0))
  seated(0) = Constant(List(0, 5, 15, 15, 25, 30, 40, 60, 65, 75))
  waiting(0) = Constant(3)

  def transition(seated: List[Int], waiting: Int): (Element[(List[Int], Int)]) = {
    val newTimes: List[Element[Int]] =
      for { time <- seated }
      yield Apply(Flip(time / 80.0), (b: Boolean) => if (b) -1 else time + 5)
    val newTimesListElem: Element[List[Int]] = Inject(newTimes:_*)
    val staying = Apply(newTimesListElem, (l: List[Int]) => l.filter(_ >= 0))

    val arriving = Poisson(2)
    val totalWaiting = Apply(arriving, (i: Int) => i + waiting)
    val placesOccupied = Apply(staying, (l: List[Int]) => l.length.min(capacity))
    val placesAvailable = Apply(placesOccupied, (i: Int) =>  capacity - i)
    val numNewlySeated = Apply(totalWaiting, placesAvailable, (tw: Int, pa: Int) => tw.min(pa))

    val newlySeated = Apply(numNewlySeated, (i: Int) => List.fill(i)(0))
    val allSeated = Apply(newlySeated, staying, (l1: List[Int], l2: List[Int]) => l1 ::: l2)
    val newWaiting = Apply(totalWaiting, numNewlySeated, (tw: Int, ns: Int) => tw - ns)
    ^^(allSeated, newWaiting)
  }

  for { step <- 1 until numSteps } {
    val newState =
      Chain(seated(step - 1), waiting(step - 1), (l: List[Int], i: Int) => transition(l, i))
    seated(step) = newState._1
    waiting(step) = newState._2
  }

  def main(args: Array[String]) {
    val alg = Importance(10000, waiting(numSteps - 1))
    alg.start()
    println(alg.probability(waiting(numSteps - 1), (i: Int) => i > 4))
    alg.kill()
  }
}

class RestaurantTest extends WordSpec with Matchers {
  Universe.createNew()
  "Restaurant" should {
    "produce a probability = 0.465 +- 0.01" taggedAs (BookExample, NonDeterministic) in {
      val alg = Importance(10000, Restaurant.waiting(Restaurant.numSteps - 1))
      alg.start()
      val prob = alg.probability(Restaurant.waiting(Restaurant.numSteps - 1), (i: Int) => i > 4)
      alg.kill()

      prob should be(0.465 +- 0.01)
    }
  }
}
