/*
 * RestaurantMonitoring.scala 
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
import com.cra.figaro.algorithm.filtering.ParticleFilter
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample
import com.cra.figaro.test.tags.NonDeterministic

object RestaurantMonitoring {
  val capacity = 10

  val initial = Universe.createNew()
  Constant(List(0, 5, 15, 15, 25, 30, 40, 60, 65, 75))("seated", initial)
  Constant(3)("waiting", initial)
  Constant(0)("arriving", initial)

  def transition(seated: List[Int], waiting: Int): (Element[(List[Int], Int, Int)]) = {
    val newTimes: List[Element[Int]] =
      for { time <- seated }
      yield Apply(Flip(time / 80.0), (b: Boolean) => if (b) -1 else time + 5)
    val newTimesListElem: Element[List[Int]] = Inject(newTimes:_*)
    val staying = Apply(newTimesListElem, (l: List[Int]) => l.filter(_ >= 0))

    val arriving = Poisson(2)
    val totalWaiting = arriving.map(_ + waiting)
    val placesOccupied = staying.map(_.length.min(capacity))
    val placesAvailable = placesOccupied.map(capacity - _)
    val numNewlySeated = Apply(totalWaiting, placesAvailable, (tw: Int, pa: Int) => tw.min(pa))

    val newlySeated = numNewlySeated.map(List.fill(_)(0))
    val allSeated = Apply(newlySeated, staying, (l1: List[Int], l2: List[Int]) => l1 ::: l2)
    val newWaiting = Apply(totalWaiting, numNewlySeated, (tw: Int, ns: Int) => tw - ns)
    ^^(allSeated, newWaiting, arriving)
  }

  def nextUniverse(previous: Universe): Universe = {
    val next = Universe.createNew()
    val previousSeated = previous.get[List[Int]]("seated")
    val previousWaiting = previous.get[Int]("waiting")
    val newState = Chain(previousSeated, previousWaiting, transition _)
    Apply(newState, (s: (List[Int], Int, Int)) => s._1)("seated", next)
    Apply(newState, (s: (List[Int], Int, Int)) => s._2)("waiting", next)
    Apply(newState, (s: (List[Int], Int, Int)) => s._3)("arriving", next)
    next
  }

  def main(args: Array[String]) {
    val arrivingObservation = List(None, None, Some(1), None, None, Some(2), None, Some(0), Some(3), None, None, None, Some(1))
    val alg = ParticleFilter(initial, nextUniverse, 10000)
    alg.start()
    for { time <- 1 to 12 } {
      val evidence = {
        arrivingObservation(time) match {
          case None => List()
          case Some(n) => List(NamedEvidence("arriving", Observation(n)))
        }
      }
      alg.advanceTime(evidence)
      print("Time " + time + ": ")
      print("expected customers = " + alg.currentExpectation("seated", (l: List[Int]) => l.length))
      println(", expected waiting = " + alg.currentExpectation("waiting", (i: Int) => i))
    }
  }
}

class RestaurantMonitoringTest extends WordSpec with Matchers {
  "Restaurant Monitoring" should {
    "produce the correct results" taggedAs (BookExample, NonDeterministic) in {
      val arrivingObservation = List(None, None, Some(1), None, None, Some(2), None, Some(0), Some(3), None, None, None, Some(1))
      val alg = ParticleFilter(RestaurantMonitoring.initial, RestaurantMonitoring.nextUniverse, 10000)
      alg.start()
      for { time <- 1 to 12 } {
        val evidence = {
          arrivingObservation(time) match {
            case None => List()
            case Some(n) => List(NamedEvidence("arriving", Observation(n)))
          }
        }
        alg.advanceTime(evidence)
        print("Time " + time + ": ")
        print("expected customers = " + alg.currentExpectation("seated", (l: List[Int]) => l.length))
        println(", expected waiting = " + alg.currentExpectation("waiting", (i: Int) => i))
      }
    }
  }
}