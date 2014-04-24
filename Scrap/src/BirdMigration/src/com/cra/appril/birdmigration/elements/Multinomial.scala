package com.cra.appril.birdmigration.elements

import scala.collection._
import com.cra.figaro.language.Apply
import com.cra.figaro.language.Constant
import com.cra.figaro.language.Element
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.language.Chain
import com.cra.figaro.algorithm.Abstraction
import com.cra.figaro.language.CachingChain
import com.cra.appril.birdmigration.model.ModelData
import com.cra.figaro.language.ElementCollection
object Multinomial {

  //Original method
  def makeIndependentBinomials(totalNumberOfTrials: Element[Int], probs: List[Element[Double]], outcomes: List[Int]): Map[Int, Element[Int]] = {
    if (probs.size != outcomes.size) {
      throw new IllegalArgumentException
    }
   // println("Make independent binomial.")
    val result = mutable.Map.empty[Int, Element[Int]]

    for ((prob, outcome) <- probs zip outcomes) {
      //val c = ElementCollection.default
      //val e : Element[Int] = CachingChain(totalNumberOfTrials, (i: Int) => if ( i > 0 ) Binomial(i, prob) else Constant(0))("BinomialX",c)
      val e = Binomial(totalNumberOfTrials, prob)
      //e.addPragma(Abstraction(5)(CounterAbstraction))
      result += outcome -> e
    }
    result.toMap
  }

  
  def makeIndependentBinomialsFixedTrials(totalNumberOfTrials: Int, probs: List[Element[Double]], outcomes: List[Int]): Map[Int, Element[Int]] = {
    if (probs.size != outcomes.size) {
      throw new IllegalArgumentException
    }
   // println("Make independent binomial.")
    val result = mutable.Map.empty[Int, Element[Int]]

    for ((prob, outcome) <- probs zip outcomes) {
      //val c = ElementCollection.default
      //val e : Element[Int] = CachingChain(totalNumberOfTrials, (i: Int) => if ( i > 0 ) Binomial(i, prob) else Constant(0))("BinomialX",c)
      val e = Binomial(totalNumberOfTrials, prob)
      //e.addPragma(Abstraction(5)(CounterAbstraction))
      result += outcome -> e
    }
    result.toMap
  }
  
  //Enforce that the number of successes in each outcome equals the total
  def makeDependentBinomials(totalNumberOfTrials: Element[Int], probs: List[Element[Double]], outcomes: List[Int]): Map[Int, Element[Int]] = {

    //Ensure index of outcome matches index of probs
    //Or do a map
    if (probs.size != outcomes.size) {
      throw new IllegalArgumentException
    }
    //println("Make dependent binomials.")
    val result = mutable.Map.empty[Int, Element[Int]]
    var previous: Element[Int] = Binomial(totalNumberOfTrials, probs(0))
    var trialsRemaining: Element[Int] = totalNumberOfTrials
    var probabilityRemaining: Element[Double] = Constant(1.0)
    result += outcomes(0) -> previous
    for (index <- 1 to probs.size - 1) {
      val trials = trialsRemaining
      val probability = probabilityRemaining
      val probabilityOfThisOutcome = Apply(probability, probs(index), (p1: Double, p2: Double) => if (p1 != 0.0) p2 / (p1) else 0.0)
      val next = Binomial(trials, probabilityOfThisOutcome)
      next.addPragma(Abstraction(6)(CounterAbstraction))
      previous = next
      //Subtract the number of success from the total number of trials

      //This is possibly a problem.
      //There's no check whether the remaining trials or probability is 0.
      trialsRemaining = Apply(trials, previous, (t: Int, i: Int) => if (t > i) t - i else 0)
      //Subtract the probability of this outcome from the total
      probabilityRemaining = Apply(probability, probabilityOfThisOutcome, (p1: Double, p2: Double) => if (p1 > p2) p1 - p2 else 0.0)
      result += outcomes(index) -> previous
    }
    result.toMap

  }

  /*
  def makeMultinomial(totalNumberOfTrials: Element[Int], probs: List[Element[Double]]) : Element[Map[Int,Element[Int]]] = {
   
    val probsInject = Inject(probs:_*)
    val multinomial : Element[Map[Int,Element[Int]]] = Chain(numberOfTrials, probsInject, (i: Int, p: List[Double]) => {
      var s = 1.0
      val result = mutable.Map.empty[Int,Element[Int]]
      var previous : Element[Int] = Binomial(i,p(0))
      result += 1 -> previous
      for (index <- 2 to p.size - 1) {
        val next = Chain(previous, (c: Int) => Binomial(i - c, s - p(index)))
        s = s - p(index)
        previous = next
        result += index -> previous
      }
      result.toMap
    })
    x
  }
  */
  

  //Make all phi and put in list
  def apply(numberOfTrials: Element[Int], probs: List[Element[Double]], outcomes: List[Int]) = makeIndependentBinomials(numberOfTrials, probs,outcomes)
  def apply(numberOfTrials: Int, probs: List[Element[Double]], outcomes: List[Int]) = makeIndependentBinomialsFixedTrials(numberOfTrials, probs,outcomes)

}