/*
 * IsingModel.scala
 * Experimental Ising-like model demonstrating use of Gibbs/VE chooser in SFI.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   August 21, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.experimental.example

import java.util.concurrent._

import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.experimental.factored.Gibbs
import com.cra.figaro.experimental.structured.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.util.random

object IsingModel {
  // Width of the NxN grid
  val width = 10

  // Each node is a vertex in the Ising model
  val causes = Array.fill(width, width)(Flip(0.5))
  val nodes = causes map (_ map (cause => {
    If(cause,
      If(Flip(0.1), Flip(0.01), Flip(0.99)),
      If(Flip(0.1), Flip(0.99), Flip(0.01))).asInstanceOf[Element[Boolean]]
  }))

  // Create edges between adjacent nodes to express the undirected model
  // Favors equality of adjacent nodes according to weight
  val weight = 2.5
  for (y <- 0 until width; x <- 1 until width) {
    val node0 = nodes(x)(y)
    val node1 = nodes(x - 1)(y)
    val constraint = node0 === node1
    constraint.addConstraint(b => if (b) weight else 1.0)
  }
  for (x <- 0 until width; y <- 1 until width) {
    val node0 = nodes(x)(y)
    val node1 = nodes(x)(y - 1)
    val constraint = node0 === node1
    constraint.addConstraint(b => if (b) weight else 1.0)
  }

  // Observe a random subset of points
  // Noisy so generating initial state for Gibbs sampler is easy
  val numObservations = 20
  println("Noisy observations:")
  for (_ <- 0 until numObservations) {
    val obs = random.nextBoolean()
    val x = random.nextInt(width)
    val y = random.nextInt(width)
    println(s"($x, $y) = $obs")
    nodes(x)(y).addConstraint(b => if (b == obs) 0.99 else 0.01)
  }
  println()

  def testAlg(alg: ProbQueryAlgorithm, test: Element[Boolean], timeOut: Long) {
    // VE may fail to complete in a reasonable amount of time, or run out of memory on this model
    // The timeout operation below just kills the algorithm if it is taking too long to prevent this
    val start = System.currentTimeMillis()

    val executor = Executors.newSingleThreadExecutor()
    val future = executor.submit(new Callable[Double] {
      def call(): Double = {
        alg.start()
        alg.probability(test, true)
      }
    })

    try {
      val prob = future.get(timeOut, TimeUnit.MILLISECONDS)
      printf("| %01.5f |  %05d  |", prob, System.currentTimeMillis() - start)
    } catch {
      case _: TimeoutException => {
        future.cancel(true);
        printf("| TIMEOUT |  %05d  |", System.currentTimeMillis() - start)
      }
    } finally {
      alg.kill()
    }
    executor.shutdownNow();
  }

  val timeOut = 30000L
  def main(args: Array[String]) {
    // For each cause, print the probability of the cause and time to compute this probability under each of these four algorithms
    println("| VE prob | VE time ||G/VE prob|G/VE time|| SG prob | SG time || G prob  | G time  |")
    for(cause <- causes.flatten) {
      testAlg(StructuredVE(cause), cause, timeOut)
      testAlg(StructuredVEGibbsChooser(0.0, 10000, cause), cause, timeOut)
      testAlg(StructuredGibbs(10000, cause), cause, timeOut)
      testAlg(Gibbs(10000, cause), cause, timeOut)
      println
    }
  }
}