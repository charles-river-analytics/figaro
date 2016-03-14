/*
 * PrinterProblemOO.scala 
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

package com.cra.figaro.test.book.chap07

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination
import org.scalatest.Matchers
import org.scalatest.WordSpec
import com.cra.figaro.test.tags.BookExample

object PrinterProblemOO {
  class Printer { //#A
    val powerButtonOn = Flip(0.95) //#A
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out) //#A
    val tonerLowIndicatorOn = //#A
      If(powerButtonOn, //#A
         CPD(tonerLevel, //#A
             'high -> Flip(0.2), //#A
             'low -> Flip(0.6), //#A
             'out -> Flip(0.99)), //#A
         Constant(false)) //#A
    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed) //#A
    val paperJamIndicatorOn = //#A
      If(powerButtonOn, //#A
         CPD(paperFlow, //#A
             'smooth -> Flip(0.1), //#A
             'uneven -> Flip(0.3), //#A
             'jammed -> Flip(0.99)), //#A
         Constant(false)) //#A
    val state = //#A
      Apply(powerButtonOn, tonerLevel, paperFlow, //#A
            (power: Boolean, toner: Symbol, paper: Symbol) => { //#A
              if (power) { //#A
                if (toner == 'high && paper == 'smooth) 'good //#A
                else if (toner == 'out || paper == 'out) 'out //#A
                else 'poor //#A
              } else 'out //#A
            }) //#A
  } //#A

  class Software { //#A
    val state = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed) //#A
  } //#A

  class Network { //#A
    val state = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down) //#A
  } //#A

  class User { //#A
    val commandCorrect = Flip(0.65) //#A
  } //#A

  class PrintExperience(printer: Printer, software: Software, network: Network, user: User) { //#B

    val numPrintedPages =
      RichCPD(user.commandCorrect, network.state, software.state, printer.state, //#C
          (*, *, *, OneOf('out)) -> Constant('zero),
          (*, *, OneOf('crashed), *) -> Constant('zero),
          (*, OneOf('down), *, *) -> Constant('zero),
          (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
          (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
      Chain(network.state, software.state, //#C
            (network: Symbol, software: Symbol) =>
              if (network == 'down || software == 'crashed) Constant(false)
              else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
              else Flip(0.9))
    val goodPrintQuality =
      CPD(printer.state, //#C
          'good -> Flip(0.95),
          'poor -> Flip(0.3),
          'out -> Constant(false))
    val summary =
      Apply(numPrintedPages, printsQuickly, goodPrintQuality, //#C
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
            if (pages == 'zero) 'none
            else if (pages == 'some || !quickly || !quality) 'poor
            else 'excellent)
  }

  val myPrinter = new Printer //#D
  val mySoftware = new Software //#D
  val myNetwork = new Network //#D
  val me = new User //#D
  val myExperience = new PrintExperience(myPrinter, mySoftware, myNetwork, me) //#D

  def step1() {
    val answerWithNoEvidence = VariableElimination.probability(myPrinter.powerButtonOn, true) //#E
    println("Prior probability the printer power button is on = " + answerWithNoEvidence)
  }

  def step2() {
    myExperience.summary.observe('poor) //#E
    val answerIfPrintResultPoor = VariableElimination.probability(myPrinter.powerButtonOn, true)
    println("Probability the printer power button is on given a poor result = " + answerIfPrintResultPoor)
  }

  def main(args: Array[String]) {
    step1()
    step2()
  }
}

class PrinterProblemOOTest extends WordSpec with Matchers {
  Universe.createNew()
  val myPrinter = new PrinterProblemOO.Printer //#D
  val mySoftware = new PrinterProblemOO.Software //#D
  val myNetwork = new PrinterProblemOO.Network //#D
  val me = new PrinterProblemOO.User //#D
  val myExperience = new PrinterProblemOO.PrintExperience(myPrinter, mySoftware, myNetwork, me) //#D
  
  "Printer Problem OO" should {
    val answerWithNoEvidence = VariableElimination.probability(myPrinter.powerButtonOn, true) //#E

    "produce a prior probability the printer power button is on = 0.95" taggedAs (BookExample) in {
      answerWithNoEvidence should be(0.95)
    }

    myExperience.summary.observe('poor) //#E
    val answerIfPrintResultPoor = VariableElimination.probability(myPrinter.powerButtonOn, true)

    "produce a probability the printer power button is on given a poor result = 1.0" taggedAs (BookExample) in {
      answerIfPrintResultPoor should be(1.0)
    }
  }
}
