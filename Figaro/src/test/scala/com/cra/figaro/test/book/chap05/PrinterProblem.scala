/*
 * PrinterProblem.scala 
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

package com.cra.figaro.test.book.chap05

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object PrinterProblem {
    val printerPowerButtonOn = Flip(0.95)
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out)
    val tonerLowIndicatorOn =
      If(printerPowerButtonOn,
         CPD(tonerLevel,
             'high -> Flip(0.2),
             'low -> Flip(0.6),
             'out -> Flip(0.99)),
         Constant(false))
    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed)
    val paperJamIndicatorOn =
      If(printerPowerButtonOn,
         CPD(paperFlow,
             'smooth -> Flip(0.1),
             'uneven -> Flip(0.3),
             'jammed -> Flip(0.99)),
         Constant(false))
    val printerState =
      Apply(printerPowerButtonOn, tonerLevel, paperFlow,
            (power: Boolean, toner: Symbol, paper: Symbol) => {
              if (power) {
                if (toner == 'high && paper == 'smooth) 'good
                else if (toner == 'out || paper == 'out) 'out
                else 'poor
              } else 'out
            })
    val softwareState = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed)
    val networkState = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down)
    val userCommandCorrect = Flip(0.65)
    val numPrintedPages =
      RichCPD(userCommandCorrect, networkState, softwareState, printerState,
          (*, *, *, OneOf('out)) -> Constant('zero),
          (*, *, OneOf('crashed), *) -> Constant('zero),
          (*, OneOf('down), *, *) -> Constant('zero),
          (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
          (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
      Chain(networkState, softwareState,
            (network: Symbol, software: Symbol) =>
              if (network == 'down || software == 'crashed) Constant(false)
              else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
              else Flip(0.9))
    val goodPrintQuality =
      CPD(printerState,
          'good -> Flip(0.95),
          'poor -> Flip(0.3),
          'out -> Constant(false))
    val printResultSummary =
      Apply(numPrintedPages, printsQuickly, goodPrintQuality,
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
            if (pages == 'zero) 'none
            else if (pages == 'some || !quickly || !quality) 'poor
            else 'excellent)

  def step1() : Double = {
    val answerWithNoEvidence = VariableElimination.probability(printerPowerButtonOn, true)
    println("Prior probability the printer power button is on = " + answerWithNoEvidence)
    answerWithNoEvidence
  }

  def step2() : Double =  {
    printResultSummary.observe('poor)
    val answerIfPrintResultPoor = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given a poor result = " + answerIfPrintResultPoor)
    answerIfPrintResultPoor
  }

  def step3() : Double =  {
    printResultSummary.observe('none)
    val answerIfPrintResultNone = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given empty result = " + answerIfPrintResultNone)
    answerIfPrintResultNone
  }

  def step4() : Double =  {
    printResultSummary.unobserve()
    printerState.observe('out)
    val answerIfPrinterStateOut = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given " + "out printer state = " + answerIfPrinterStateOut)
    answerIfPrinterStateOut
  }

  def step4a() : Double =  {
    printResultSummary.observe('none)
    val answerIfPrinterStateOutAndResultNone = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given out printer state and empty result = " + answerIfPrinterStateOutAndResultNone)
    answerIfPrinterStateOutAndResultNone
  }  
  
  def step5() : Double =  {
    printResultSummary.unobserve()
    printerState.unobserve()
    val printerStateGoodPrior = VariableElimination.probability(printerState, 'good)
    println("Prior probability the printer state is good = " + printerStateGoodPrior)
    printerStateGoodPrior
  }

  def step5a() : Double =  {
    tonerLowIndicatorOn.observe(true)
    val printerStateGoodGivenTonerLowIndicatorOn = VariableElimination.probability(printerState, 'good)
    println("Probability printer state is good given low toner indicator = " + printerStateGoodGivenTonerLowIndicatorOn)
    printerStateGoodGivenTonerLowIndicatorOn
  }

  def step6() : Double =  {
    tonerLowIndicatorOn.unobserve()
    val softwareStateCorrectPrior = VariableElimination.probability(softwareState, 'correct)
    println("Prior probability the software state is correct = " + softwareStateCorrectPrior)
    softwareStateCorrectPrior
  }

  def step6a() : Double =  {
    networkState.observe('up)
    val softwareStateCorrectGivenNetworkUp = VariableElimination.probability(softwareState, 'correct)
    println("Probability software state is correct given network up = " + softwareStateCorrectGivenNetworkUp)
    softwareStateCorrectGivenNetworkUp
  }

  def step7() : Double =  {
    networkState.unobserve()
    printsQuickly.observe(false)
    val softwareStateCorrectGivenPrintsSlowly = VariableElimination.probability(softwareState, 'correct)
    println("Probability software state is correct given prints slowly = " + softwareStateCorrectGivenPrintsSlowly)
    softwareStateCorrectGivenPrintsSlowly
  }

  def step7a() : Double =  {
    networkState.observe('up)
    val softwareStateCorrectGivenPrintsSlowlyAndNetworkUp = VariableElimination.probability(softwareState, 'correct)
    println("Probability software state is correct given prints slowly and network up = " + softwareStateCorrectGivenPrintsSlowlyAndNetworkUp)
    softwareStateCorrectGivenPrintsSlowlyAndNetworkUp
  }  
  
  def main(args: Array[String]) {
    step1()
    step2()
    step3()
    step4()
    step4a()
    step5()
    step5a()
    step6()
    step6a()
    step7()
    step7a()
  }
}

class PrinterProblemTest extends WordSpec with Matchers {
  Universe.createNew()
  "Printer Problem" should {
    "answerWithNoEvidence equals 0.95" taggedAs (BookExample) in {
      PrinterProblem.step1() should be(0.95)
    }
    "answerIfPrintResultPoor equals 1.0" taggedAs (BookExample) in {
      PrinterProblem.step2() should be(1.0)
    }
    "answerIfPrintResultNone equals 0.8573402523786461" taggedAs (BookExample) in {
      PrinterProblem.step3() should be(0.8573402523786461)
    }
    "answerIfPrinterStateOut equals 0.6551724137931032" taggedAs (BookExample) in {
      PrinterProblem.step4() should be(0.6551724137931032)
    }
    "answerIfPrinterStateOutAndResultNone equals 0.6551724137931033" taggedAs (BookExample) in {
      PrinterProblem.step4a() should be(0.6551724137931033)
    }
    "printerStateGoodPrior equals 0.39899999999999997" taggedAs (BookExample) in {
      PrinterProblem.step5() should be(0.39899999999999997)
    }
    "printerStateGoodGivenTonerLowIndicatorOn approximately equals 0.2339832869" taggedAs (BookExample) in {
      PrinterProblem.step5a() should be(0.2339832869 +- 0.00000000001)
    }
    "softwareStateCorrectPrior equals 0.8" taggedAs (BookExample) in {
      PrinterProblem.step6() should be(0.8)
    }
    "softwareStateCorrectGivenNetworkUp equals 0.7999999999999999" taggedAs (BookExample) in {
      PrinterProblem.step6a() should be(0.7999999999999999)
    }
    "softwareStateCorrectGivenPrintsSlowly equals 0.6197991391678623" taggedAs (BookExample) in {
      PrinterProblem.step7() should be(0.6197991391678623)
    }
    "softwareStateCorrectGivenPrintsSlowlyAndNetworkUp equals 0.39024390243902435" taggedAs (BookExample) in {
      PrinterProblem.step7a() should be(0.39024390243902435)
    }
  }
}
