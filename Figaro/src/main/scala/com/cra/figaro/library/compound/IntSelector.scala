/*
 * IntSelector.scala
 * Selection of an integer uniformly from 0 to a variable upper bound.
 * 
 * Created By:      Avi Pfeffer (apfeffer@cra.com)
 * Creation Date:   Oct 17, 2011
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.library.compound

import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.lazyfactored._

/**
 * An IntSelector represents the selection of an integer from 0 (inclusive) to a variable upper bound (exclusive). The
 * upper bound is an element represented by the bound argument. The IntSelector class has been defined so that
 * (1) the value is always uniformly distributed in the range, no matter how the upper bound changes, and
 * (2) it attempts to avoid changing the value as much as it possibly can as the upper bound changes.
 * This latter property makes the class useful in an algorithm like Metropolis-Hastings, where we would like to
 * change as little as possible as we make a proposal.
 */
class IntSelector(name: Name[Int], counter: Element[Int], collection: ElementCollection)
  extends Element[Int](name, collection) with IfArgsCacheable[Int] with ValuesMaker[Int] with ProbFactorMaker {
  // We achieve the two properties by making the randomness a random stream of doubles and selecting the index
  // within range that has the highest randomness. If the bound changes, the double associated with the index
  // does not change, so quite often the highest index will stay the same.
  type Randomness = Stream[Double]

  def args = List(counter)

  def generateRandomness(): Randomness = Stream.continually(random.nextDouble())

  def generateValue(rand: Randomness): Int = argmax(rand take counter.value)

  def makeValues(depth: Int): ValueSet[Int] = {
    val counterValues = LazyValues(universe)(counter, depth - 1)
    if (counterValues.regularValues.nonEmpty) {
      val maxCounter = counterValues.regularValues.max
      val all = List.tabulate(maxCounter)(i => i).toSet
      if (counterValues.hasStar) ValueSet.withStar(all); else ValueSet.withoutStar(all)
    } else { ValueSet.withStar(Set()) }
  }

  def makeFactors: List[Factor[Double]] = {
    val thisVar = Variable(this)
    val counterVar = Variable(counter)
    val comb = Factory.make[Double](List(thisVar, counterVar))
    comb.fillByRule((l: List[Any]) => {
      val xvalue0 :: xvalue1 :: _ = l.asInstanceOf[List[Extended[Int]]]
      if (xvalue0.isRegular && xvalue1.isRegular) {
        if (xvalue0.value < xvalue1.value) 1.0/xvalue1.value; else 0.0  
      } else 1.0
      
    })
    List(comb)
  }
}

object IntSelector {
  /**
   * Create an IntSelector using the counter element as the upper bound
   */
  def apply(counter: Element[Int])(implicit name: Name[Int], collection: ElementCollection) =
    new IntSelector(name, counter, collection)
}
