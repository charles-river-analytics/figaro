/*
 * PrinterProblemTypeUncertainty.scala 
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

object PrinterProblemTypeUncertainty extends ElementCollection {
  abstract class Printer extends ElementCollection {
    val powerButtonOn = Flip(0.95)("power button on", this)

    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed)
    val paperJamIndicatorOn =
      If(powerButtonOn,
         CPD(paperFlow,
             'smooth -> Flip(0.1),
             'uneven -> Flip(0.3),
             'jammed -> Flip(0.99)),
         Constant(false))

    val state: Element[Symbol]
  }

  class LaserPrinter extends Printer {
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out)
    val tonerLowIndicatorOn =
      If(powerButtonOn,
         CPD(tonerLevel,
             'high -> Flip(0.2),
             'low -> Flip(0.6),
             'out -> Flip(0.99)),
         Constant(false))
    val state =
      Apply(powerButtonOn, tonerLevel, paperFlow,
            (power: Boolean, toner: Symbol, paper: Symbol) => {
              if (power) {
                if (toner == 'high && paper == 'smooth) 'good
                else if (toner == 'out || paper == 'out) 'out
                else 'poor
              } else 'out
            }
      )
  }

  class InkjetPrinter extends Printer {
    val inkCartridgeEmpty = Flip(0.1)
    val inkCartridgeEmptyIndicator = If(inkCartridgeEmpty, Flip(0.99), Flip(0.3))
    val cloggedNozzle = Flip(0.001)
    val state =
      Apply(powerButtonOn, inkCartridgeEmpty, cloggedNozzle, paperFlow,
            (power: Boolean, ink: Boolean, nozzle: Boolean, paper: Symbol) => {
              if (power && !ink && !nozzle) {
                if (paper == 'smooth) 'good
                else if (paper == 'uneven) 'poor
                else 'out
              } else 'out
            }
      )
  }

  class Software {
    val state = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed)
  }

  class Network {
    val state = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down)
  }

  class User {
    val commandCorrect = Flip(0.65)
  }

  class PrintExperience(printer: Printer, software: Software, network: Network, user: User) extends ElementCollection {
    val numPrintedPages =
      RichCPD(user.commandCorrect, network.state, software.state, printer.state,
          (*, *, *, OneOf('out)) -> Constant('zero),
          (*, *, OneOf('crashed), *) -> Constant('zero),
          (*, OneOf('down), *, *) -> Constant('zero),
          (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
          (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
      Chain(network.state, software.state,
            (network: Symbol, software: Symbol) =>
              if (network == 'down || software == 'crashed) Constant(false)
              else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
              else Flip(0.9))
    val goodPrintQuality =
      CPD(printer.state,
          'good -> Flip(0.95),
          'poor -> Flip(0.3),
          'out -> Constant(false))
    val summary =
      Apply(numPrintedPages, printsQuickly, goodPrintQuality,
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
            if (pages == 'zero) 'none
            else if (pages == 'some || !quickly || !quality) 'poor
            else 'excellent)("summary", this)
  }

  val myPrinter = Select(0.3 -> new LaserPrinter, 0.7 -> new InkjetPrinter)("my printer", this)
  val mySoftware = new Software
  val myNetwork = new Network
  val me = new User
  val myExperience =
    Apply(myPrinter, (p: Printer) => new PrintExperience(p, mySoftware, myNetwork, me))("print experience", this)

  def step1() {
    val summary = get[Symbol]("print experience.summary")
    summary.observe('none)

    val powerButtonOn = get[Boolean]("my printer.power button on")
    val isLaser = Apply(myPrinter, (p: Printer) => p.isInstanceOf[LaserPrinter])
    val alg = VariableElimination(powerButtonOn, isLaser)
    alg.start()
    println("After observing no print result:")
    println("Probability printer power button is on = " + alg.probability(powerButtonOn, true))
    println("Probability printer is a laser printer = " + alg.probability(isLaser, true))
    alg.kill()
  }

  def main(args: Array[String]) {
    step1()
  }
}

class PrinterProblemTypeUncertaintyTest extends WordSpec with Matchers {
  Universe.createNew()
  val summary = PrinterProblemTypeUncertainty.get[Symbol]("print experience.summary")
  summary.observe('none)
  
  val powerButtonOn = PrinterProblemTypeUncertainty.get[Boolean]("my printer.power button on")
  val isLaser = Apply(PrinterProblemTypeUncertainty.myPrinter, (p: PrinterProblemTypeUncertainty.Printer) => p.isInstanceOf[PrinterProblemTypeUncertainty.LaserPrinter])
  val alg = VariableElimination(powerButtonOn, isLaser)
  alg.start()
  val btnOn = alg.probability(powerButtonOn, true)
  val laserPrinter = alg.probability(isLaser, true)
  alg.kill()
      
  "Printer Problem Type Uncertainty" should {
    "after observing no print result:" should {
      "produce a probability printer power button is on = 0.8868215502107176" taggedAs (BookExample) in {
        btnOn should be(0.8868215502107176)
      }
      "produce a probability printer is a laser printer = 0.2380036100085 +- 0.00000000000001" taggedAs (BookExample) in {
        laserPrinter should be(0.2380036100085 +- 0.00000000000001)
      }
    }
  }
}
