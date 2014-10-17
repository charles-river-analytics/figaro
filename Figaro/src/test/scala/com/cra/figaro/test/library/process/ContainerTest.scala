package com.cra.figaro.test.library.process

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.library.process._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound._

class ContainerTest extends WordSpec with Matchers {
  "A Container" should {
    "check range correctly" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      proc.rangeCheck(2) should equal (true)
      proc.rangeCheck(1) should equal (false)
    }
    
    "when mapping, have each point mapped according to the function" in {
      Universe.createNew()
      val proc = createContainer(List(2,3)).map(!_)
      val elem = proc(3)
      VariableElimination.probability(elem, true) should be ((2.0 / 3) +- 0.000000001)
    }
    
    "when chaining, have each point flatMapped according to the function" in {
      Universe.createNew()
      val proc = createContainer(List(2,3)).chain(if (_) Flip(0.3) else Flip(0.6))
      val elem = proc(3)
      VariableElimination.probability(elem, true) should be ((1.0 / 3 * 0.3 + 2.0 / 3 * 0.6) +- 0.000000001)
    }
    
    "when appending, have all the elements of both processes, with the second process replacing the first when necessary" in {
      Universe.createNew()
      val proc1 = createContainer(List(2,3))
      val proc2 = createContainer(List(3,4), true)
      val proc = proc1 ++ proc2
      val elem2 = proc(2)
      val elem3 = proc(3)
      val elem4 = proc(4)
      an [proc.IndexOutOfRangeException] should be thrownBy { proc(1) }
      val alg = VariableElimination(elem2, elem3, elem4)
      alg.start()
      alg.probability(elem2, true) should be (0.5 +- 0.000001)
      alg.probability(elem3, true) should be (2.0 / 3.0 +- 0.00000001) // inverted
      alg.probability(elem4, true) should be (3.0 / 4.0 +- 0.00000001) // inverted
      alg.kill()
    }
    
    "when folding or reducing, have the points folded according to the function" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elem1 = proc.foldLeft(true)((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem2 = proc.foldRight(true)((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem3 = proc.reduce((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem4 = proc.aggregate(true)((b1: Boolean, b2: Boolean) => !b1 || b2, (b1: Boolean, b2: Boolean) => b1 && b2)
      val alg = Importance(10000, elem1, elem2, elem3, elem4)
      alg.start()
      alg.probability(elem1, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.probability(elem2, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.probability(elem3, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.probability(elem4, true) should be (((1.0 / 2.0) * (1.0 / 3.0)) +- 0.02)
      alg.kill()
    }
    
    "when quantifying over elements satisfying a predicate, produce the right answer" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elem1 = proc.count((b: Boolean) => b)
      val elem2 = proc.exists((b: Boolean) => b)
      val elem3 = proc.forall((b: Boolean) => b)
      val alg = Importance(10000, elem1, elem2, elem3)
      alg.start()
      val p0 = 1.0 / 2 * 2.0 / 3
      val p1 = 1.0 / 2 * 1.0 / 3 + 1.0 / 2 * 2.0 / 3
      val p2 = 1.0 / 2 * 1.0 / 3
      alg.probability(elem1, 0) should be (p0 +- 0.02)
      alg.probability(elem1, 1) should be (p1 +- 0.02)
      alg.probability(elem1, 2) should be (p2 +- 0.02)
      alg.probability(elem2, true) should be ((p1 + p2) +- 0.02)
      alg.probability(elem3, true) should be (p2 +- 0.02)
      alg.kill()
    }
    
    "when finding the index of the first element, produce the right answer" in {
      Universe.createNew()
      val proc = createContainer(List(2,3))
      val elem = proc.findIndex((b: Boolean) => b)
      val alg = Importance(10000, elem)
      alg.start()
      val p2 = 1.0 / 2.0
      val p3 = (1 - p2) * 1.0 / 3.0
      val pNone = 1 - p2 - p3
      alg.probability(elem, Some(2)) should be (p2 +- 0.02)
      alg.probability(elem, Some(3)) should be (p3 +- 0.02)
      alg.probability(elem, None) should be (pNone +- 0.02)
      alg.kill()
    }
    
  }
  
  def createContainer(is: List[Int], invert: Boolean = false): Container[Int, Boolean] = new Container[Int, Boolean] {
    val indices = is
    def generate(index: Int) = if (invert) Flip(1.0 - 1.0 / index) else Flip(1.0 / index)
    def generate(indices: List[Int]) = {
      val unary = for {
        index <- indices
      } yield (index, generate(index))
      val map = Map(unary:_*)
      val binary = 
        for {
          index1 <- indices
          index2 <- indices
          if index1 < index2
        } yield {
          val elem1 = map(index1)
          val elem2 = map(index2)
          val pair = ^^(elem1, elem2)
          pair.addConstraint((pair: (Boolean, Boolean)) => if (pair._1 != pair._2) 1.0 / (index1 + index2) else 1.0)
          pair
        }
      Map(unary:_*)  
    }
  }
}