package com.cra.figaro.test.library.process

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.cra.figaro.library.process._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound._

class ProcessTest extends WordSpec with Matchers {
  "A process" should {
    "get the right element for an index in range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elem = proc(2)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be (0.5 +- 0.0000000001)
      alg.kill()
    }
    
    "get the right elements for multiple indices in range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elems = proc(List(2,3))
      val alg = VariableElimination(elems(2))
      alg.start()
      val q23 = (1.0 / 2) * (1.0 / 3)
      val q2not3 = (1.0 / 2) * (2.0 / 3) * (1.0 / 5)
      val qnot23 = (1.0 / 2) * (1.0 / 3) * (1.0 / 5)
      val qnot2not3 = (1.0 / 2) * (2.0 / 3)
      val q2 = q23 + q2not3
      val qnot2 = qnot23 + qnot2not3
      val p2 = q2 / (q2 + qnot2)
      alg.probability(elems(2), true) should be (p2 +- 0.0000000001)
      alg.kill()
    }

    "throw IndexOutOfRange exception for an index out of range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      an [proc.IndexOutOfRangeException] should be thrownBy { proc(1) }
      an [proc.IndexOutOfRangeException] should be thrownBy { proc(List(2,1)) }
    }
    
    "get the right optional element for an index in or out of range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elem1 = proc.get(2)
      val elem2 = proc.get(1)
      val alg = VariableElimination(elem1, elem2)
      alg.start()
      alg.probability(elem1, Some(true)) should be (0.5 +- 0.00000001)
      alg.probability(elem2, None) should be (1.0 +- 0.000000001)
      alg.kill()
    }
    
    "get the right optional elements for multiple indices in or out of range" in {
      Universe.createNew()
      val proc = createProcess(List(2,3))
      val elems = proc.get(List(2, 1))
      val alg = VariableElimination(elems(1), elems(2))
      alg.start()
      alg.probability(elems(2), Some(true)) should be (0.5 +- 0.00000001)
      alg.probability(elems(1), None) should be (1.0 +- 0.000000001)
      alg.kill()
    }
    
    "when mapping, have each point mapped according to the function" in {
      Universe.createNew()
      val proc = createProcess(List(2,3)).map(!_)
      val elem = proc(3)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be ((2.0 / 3) +- 0.000000001)
      alg.kill()
    }
    
    "when chaining, have each point flatMapped according to the function" in {
      Universe.createNew()
      val proc = createProcess(List(2,3)).chain(if (_) Flip(0.3) else Flip(0.6))
      val elem = proc(3)
      val alg = VariableElimination(elem)
      alg.start()
      alg.probability(elem, true) should be ((1.0 / 3 * 0.3 + 2.0 / 3 * 0.6) +- 0.000000001)
      alg.kill()
    }
    
    "when appending, have all the elements of both processes, with the second process replacing the first when necessary" in {
      Universe.createNew()
      val proc1 = createProcess(List(2,3))
      val proc2 = createProcess(List(3,4), true)
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
  }
  /*
  "A fixed independent array" should {
    "get the correct elements at individual points" in {
      Universe.createNew()
      val array = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val elem0 = array(0)
      val elem3 = array(3)
      assert (elem0.isInstanceOf[AtomicFlip])
      elem0.asInstanceOf[AtomicFlip].prob should be(1.0 +- 0.000000000001)
      assert (elem3.isInstanceOf[AtomicFlip])
      elem3.asInstanceOf[AtomicFlip].prob should be(0.25 +- 0.00000000001)
    }
    
    "range check an element" in {
      Universe.createNew()
      val array = new FixedIndependentArray(4, (i: Int) => Flip(random.nextDouble))
      an [array.IndexOutOfRangeException] should be thrownBy { array(-1) } 
      an [array.IndexOutOfRangeException] should be thrownBy { array(4) } 
    }
    
    "safely get an optional element using get" in {
      Universe.createNew()
      val array = new FixedIndependentArray(4, (i: Int) => Constant(i))
      val elem0 = array.get(0)
      val elem4 = array.get(4)
      val alg0 = VariableElimination(elem0)
      alg0.start()
      alg0.probability(elem0, Some(0)) should be (1.0 +- 0.0000001)
      alg0.kill()
      val alg4 = VariableElimination(elem4)
      alg4.start()
      alg4.probability(elem4, None) should be (1.0 +- 0.0000001)
      alg4.kill()
    }
    
    "when mapping, have each point mapped according to the function" in {
      Universe.createNew()
      val array1 = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2 = array1.map((b: Boolean) => !b)
      val elem0 = array2(0)
      val elem3 = array2(3)
      val algorithm = VariableElimination(elem0, elem3)
      algorithm.start()
      algorithm.probability(elem0, true) should be (0.0 +- 0.000000001)
      algorithm.probability(elem3, true) should be (0.75 +- 0.000000001)
      algorithm.kill() 
    }

    "when chaining, have each point mapped according to the function" in {
      Universe.createNew()
      val array1 = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2 = array1.chain((b: Boolean) => if (b) Constant(1); else Constant(2))
      val elem0 = array2(0)
      val elem3 = array2(3)
      val algorithm = VariableElimination(elem0, elem3)
      algorithm.start()
      algorithm.probability(elem0, 2) should be (0.0 +- 0.000000001)
      algorithm.probability(elem3, 2) should be (0.75 +- 0.000000001)
      algorithm.kill() 
    }

    "when folding or reducing, have the points folded according to the function" in {
      Universe.createNew()
      val array1 = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val elem2 = array1.foldLeft(true)((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem3 = array1.foldRight(true)((b1: Boolean, b2: Boolean) => b1 && b2)
      val elem4 = array1.reduce((b1: Boolean, b2: Boolean) => b1 && b2)
      val alg2 = Importance(10000, elem2)
      alg2.start()
      alg2.probability(elem2, true) should be (((1.0 / 1.0) * (1.0 / 2.0)  * (1.0 / 3.0) * (1.0 / 4.0)) +- 0.01)
      alg2.kill()
      val alg3 = Importance(10000, elem3)
      alg3.start()
      alg3.probability(elem3, true) should be (((1.0 / 1.0) * (1.0 / 2.0)  * (1.0 / 3.0) * (1.0 / 4.0)) +- 0.01)
      alg3.kill()
      val alg4 = Importance(10000, elem4)
      alg4.start()
      alg4.probability(elem4, true) should be (((1.0 / 1.0) * (1.0 / 2.0)  * (1.0 / 3.0) * (1.0 / 4.0)) +- 0.01)
      alg4.kill()
    }
  }
  * 
  */
  /*
  "A process element" should {
    "create an element over values at a particular index when calling get" in {
      Universe.createNew()
      val array1: Container[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: Container[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem = new ContainerElement[Int, Boolean](switch)
      val elem1 = procElem.get(3)
      val alg = VariableElimination(elem1)
      alg.start()
      alg.probability(elem1, Some(true)) should be ((0.2 * 1.0 / 4.0 + 0.8 * 3.0 / 4.0) +- 0.0000000001)
      alg.kill()
    }
    
    "map a function pointwise through the process" in {
      val array1: Container[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: Container[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem1 = new ContainerElement[Int, Boolean](switch)
      val procElem2 = procElem1.map((b: Boolean) => !b)
      val elem1 = procElem2.get(3)
      val alg = VariableElimination(elem1)
      alg.start()
      alg.probability(elem1, Some(false)) should be ((0.2 * 1.0 / 4.0 + 0.8 * 3.0 / 4.0) +- 0.0000000001)
      alg.kill()
    }

    "chain a function pointwise through the process" in {
      val array1: Container[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: Container[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem1 = new ContainerElement[Int, Boolean](switch)
      val procElem2 = procElem1.chain((b: Boolean) => if (b) Constant(1); else Constant(2))
      val elem1 = procElem2.get(3)
      val alg = VariableElimination(elem1)
      alg.start()
      alg.probability(elem1, Some(1)) should be ((0.2 * 1.0 / 4.0 + 0.8 * 3.0 / 4.0) +- 0.0000000001)
      alg.kill()
    }
  }
*/
  /*
  "A finite process element" should {
    "create an element over values at a particular index when calling get" in {
      Universe.createNew()
      val array1: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem = new FiniteContainerElement[Int, Boolean](switch)
      val elem1 = procElem.get(3)
      val alg = VariableElimination(elem1)
      alg.start()
      alg.probability(elem1, Some(true)) should be ((0.2 * 1.0 / 4.0 + 0.8 * 3.0 / 4.0) +- 0.0000000001)
      alg.kill()
    }
    
    "map a function pointwise through the process" in {
      Universe.createNew()
      val array1: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem1 = new FiniteContainerElement[Int, Boolean](switch)
      val procElem2 = procElem1.map((b: Boolean) => !b)
      val elem1 = procElem2.get(3)
      val alg = VariableElimination(elem1)
      alg.start()
      alg.probability(elem1, Some(false)) should be ((0.2 * 1.0 / 4.0 + 0.8 * 3.0 / 4.0) +- 0.0000000001)
      alg.kill()
    }

    "chain a function pointwise through the process" in {
      Universe.createNew()
      val array1: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem1 = new FiniteContainerElement[Int, Boolean](switch)
      val procElem2 = procElem1.chain((b: Boolean) => if (b) Constant(1); else Constant(2))
      val elem1 = procElem2.get(3)
      val alg = VariableElimination(elem1)
      alg.start()
      alg.probability(elem1, Some(1)) should be ((0.2 * 1.0 / 4.0 + 0.8 * 3.0 / 4.0) +- 0.0000000001)
      alg.kill()
    }
    
    "fold and reduce pointwise through the process" in {
      Universe.createNew()
      val array1: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 / (i + 1)))
      val array2: FiniteContainer[Int, Boolean] = new FixedIndependentArray(4, (i: Int) => Flip(1.0 - 1.0 / (i + 1)))
      val switch = Select(0.2 -> array1, 0.8 -> array2)
      val procElem = new FiniteContainerElement[Int, Boolean](switch)
      val elem2 = procElem.foldLeft(true, (b1: Boolean, b2: Boolean) => b1 && b2)
      val elem3 = procElem.foldRight(true, (b1: Boolean, b2: Boolean) => b1 && b2)
      val elem4 = procElem.reduce((b1: Boolean, b2: Boolean) => b1 && b2)
      val alg2 = Importance(10000, elem2)
      alg2.start()
      // With probability 0.8, array2 is chosen, which are never all true because array2(0) is Flip(0)
      alg2.probability(elem2, true) should be ((0.2 * (1.0 / 1.0) * (1.0 / 2.0)  * (1.0 / 3.0) * (1.0 / 4.0)) +- 0.01)
      alg2.kill()
      val alg3 = Importance(10000, elem3)
      alg3.start()
      // With probability 0.8, array2 is chosen, which are never all true because array2(0) is Flip(0)
      alg3.probability(elem3, true) should be ((0.2 * (1.0 / 1.0) * (1.0 / 2.0)  * (1.0 / 3.0) * (1.0 / 4.0)) +- 0.01)
      alg3.kill()
      val alg4 = Importance(10000, elem4)
      alg4.start()
      // With probability 0.8, array2 is chosen, which are never all true because array2(0) is Flip(0)
      alg4.probability(elem4, true) should be ((0.2 * (1.0 / 1.0) * (1.0 / 2.0)  * (1.0 / 3.0) * (1.0 / 4.0)) +- 0.01)
      alg4.kill()
    }
  }
  */
  /*
  "Making an array of independent elements of variable size" should {
    "have the correct size distribution" in {
      Universe.createNew()
      val numItems = Select(0.4 -> 1, 0.6 -> 2)
      def generator(i: Int): Element[Int] = Select(0.2 -> (i + 1), 0.8 -> (i + 2))
      val array = VariableSizeArray(numItems, generator)
      val count = array.foldLeft(0, (i1: Int, i2: Int) => i1 + 1)
      val alg = Importance(1000, count)
      alg.start()
      alg.probability(count, 1) should be (0.4 +- 0.05)
      alg.probability(count, 2) should be (0.6 +- 0.05)
    } 
    
    "have the correct distribution over an element" in {
      Universe.createNew()
      val numItems = Select(0.4 -> 1, 0.6 -> 2)
      def generator(i: Int): Element[Int] = Select(0.2 -> (i + 1), 0.8 -> (i + 2))
      val array = VariableSizeArray(numItems, generator)
      val elem1 = array.get(1)
      val alg = Importance(1000, elem1)
      alg.start()
      alg.probability(elem1, None) should be (0.4 +- 0.05)
      alg.probability(elem1, Some(2)) should be (0.6 * 0.2 +- 0.05)
      alg.probability(elem1, Some(3)) should be (0.6 * 0.8 +- 0.05)
    }
    
    "have the correct aggregate" in {
      Universe.createNew()
      val numItems = Select(0.4 -> 1, 0.6 -> 2)
      def generator(i: Int): Element[Int] = Select(0.2 -> (i + 1), 0.8 -> (i + 2))
      val array = VariableSizeArray(numItems, generator)
      val total = array.reduce(_ + _)
      val alg = Importance(1000, total)
      alg.start()
      val p1 = 0.4 * 0.2
      val p2 = 0.4 * 0.8
      val p12 = 0.6 * 0.2 * 0.2
      val p13 = 0.6 * 0.2 * 0.8
      val p22 = 0.6 * 0.8 * 0.2
      val p23 = 0.6 * 0.8 * 0.8
      val answer = p1 * 1 + p2 * 2 + p12 * 3 + p13 * 4 + p22 * 4 + p23 * 5
      alg.expectation(total, (i: Int) => i.toDouble) should be (answer +- 0.15)
    }
  }
  * 
  */
  
  def createProcess(indices: List[Int], invert: Boolean = false): Process[Int, Boolean] = new Process[Int, Boolean] {
    def rangeCheck(index: Int) = indices.contains(index)
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