/*
 * LogStatistics.scala
 * Tests for statistics in log space.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Aug 22, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.util

import com.cra.figaro.util._
import com.cra.figaro.util.LogStatistics.oneSidedTTest
import org.scalatest.{Matchers, WordSpec}

class LogStatisticsTest extends WordSpec with Matchers {
  // Used to test underflow/overflow
  val logOffset = 100000

  "Running a one-sided t-test" should {
    "throw an exception" when {
      "one of the counts is less than 2" in {
        val v1 = LogStatistics(0.0, 1.0, 1)
        val v2 = LogStatistics(1.0, 1.0, 30)
        an [IllegalArgumentException] shouldBe thrownBy(oneSidedTTest(v1, v2))
        an [IllegalArgumentException] shouldBe thrownBy(oneSidedTTest(v2, v1))
      }
    }

    "handle 0 variance" when {
      "one variance is 0" in {
        val v1 = LogStatistics(0.0, Double.NegativeInfinity, 20)
        val v2 = LogStatistics(Double.NegativeInfinity, 0.0, 20)
        val v3 = LogStatistics(0.2, 0.0, 20)
        val v4 = LogStatistics(0.0, 0.0, 20)

        oneSidedTTest(v1, v2) should be (1.3060E-4 +- 1E-8)
        oneSidedTTest(v1, v3) should be (0.16727 +- 1E-5)
        oneSidedTTest(v1, v4) should be (0.5 +- 1E-5)
      }

      "both variances are 0" in {
        val v1 = LogStatistics(0.0, Double.NegativeInfinity, 20)
        val v2 = LogStatistics(Double.NegativeInfinity, Double.NegativeInfinity, 20)

        oneSidedTTest(v1, v2) should be (0.0 +- 1E-5)
        oneSidedTTest(v1, v1) should be (0.5 +- 1E-5)
        oneSidedTTest(v2, v2) should be (0.5 +- 1E-5)
      }
    }

    "handle 0 mean" when {
      "one mean is 0" in {
        val v1 = LogStatistics(Double.NegativeInfinity, 0.0, 20)
        val v2 = LogStatistics(0.0, 1.0, 20)

        oneSidedTTest(v1, v2) should be (0.013536 +- 1E-6)
      }

      "both means are 0" in {
        val v1 = LogStatistics(Double.NegativeInfinity, 0.0, 20)
        val v2 = LogStatistics(Double.NegativeInfinity, 1.0, 20)

        oneSidedTTest(v1, v2) should be (0.5 +- 1E-5)
      }
    }

    "compute degrees of freedom" when {
      "the counts are the same" in {
        val v1 = LogStatistics(0.0, 0.0, 5)
        val v2 = LogStatistics(0.1, 0.1, 5)

        oneSidedTTest(v1, v2) should be (0.43763 +- 1E-5)
      }

      "the counts are different" in {
        val v1 = LogStatistics(0.0, 0.0, 3)
        val v2 = LogStatistics(0.1, 0.1, 7)

        oneSidedTTest(v1, v2) should be (0.44396 +- 1E-5)
      }
    }

    "compare the lesser mean to the greater mean" when {
      "the means are the same" in {
        val v1 = LogStatistics(0.0, 0.0, 20)
        val v2 = LogStatistics(0.0, 0.1, 30)

        oneSidedTTest(v1, v2) should be (0.5 +- 1E-5)
        oneSidedTTest(v2, v1) should be (0.5 +- 1E-5)
      }

      "the means are different" in {
        val v1 = LogStatistics(0.0, 0.0, 20)
        val v2 = LogStatistics(0.1, 0.1, 30)

        oneSidedTTest(v1, v2) should be (0.36147 +- 1E-5)
        oneSidedTTest(v2, v1) should be (0.36147 +- 1E-5)
      }
    }

    "remain stable in log space" when {
      "exponentiation could underflow" in {
        val v1 = LogStatistics(0.0, 0.0, 20).multiplyByConstant(-logOffset)
        val v2 = LogStatistics(0.1, 0.1, 30).multiplyByConstant(-logOffset)

        oneSidedTTest(v1, v2) should be (0.36147 +- 1E-5)
      }

      "exponentiation could overflow" in {
        val v1 = LogStatistics(0.0, 0.0, 20).multiplyByConstant(logOffset)
        val v2 = LogStatistics(0.1, 0.1, 30).multiplyByConstant(logOffset)

        oneSidedTTest(v1, v2) should be (0.36147 +- 1E-5)
      }
    }
  }

  "Running online log statistics" should {
    "handle 0 observations" when {
      "all of the observations are 0" in {
        val ols = new OnlineLogStatistics {}
        val numObservations = 100
        for(_ <- 1 to numObservations) ols.record(Double.NegativeInfinity)

        val logStats = ols.totalLogStatistics
        logStats.logMean should be (Double.NegativeInfinity)
        logStats.logVariance should be (Double.NegativeInfinity)
        logStats.count should be (numObservations)
      }

      "some of the observations are 0" in {
        val ols = new OnlineLogStatistics {}
        val observations = List(Double.NegativeInfinity, 0.0, -0.5, Double.NegativeInfinity, -2.2, 0.1)
        observations.foreach(ols.record)

        val logStats = ols.totalLogStatistics
        logStats.logMean should be (-0.75413 +- 1E-5)
        logStats.logVariance should be (-1.3674 +- 1E-4)
      }
    }

    "compute the correct result" when {
      "given no observations" in {
        val ols = new OnlineLogStatistics {}

        val logStats = ols.totalLogStatistics
        logStats.logMean should be (Double.NegativeInfinity)
        logStats.logVariance.isNaN should be (true)
        logStats.count should be (0)
      }

      "given one observation" in {
        val ols = new OnlineLogStatistics {}
        ols.record(1.5)

        val logStats = ols.totalLogStatistics
        logStats.logMean should be (1.5 +- 1E-4)
        logStats.logVariance.isNaN should be (true)
        logStats.count should be (1)
      }

      "given several observations" in {
        val ols = new OnlineLogStatistics {}
        val observations = List(-3.7, 2.1, 0.8, 1.2, 0.8, -0.5)
        observations.foreach(ols.record)

        val logStats = ols.totalLogStatistics
        logStats.logMean should be (1.0158 +- 1E-4)
        logStats.logVariance should be (2.1337 +- 1E-4)
        logStats.count should be (observations.length)
      }
    }

    "remain stable in log space" when {
      "exponentiation could underflow" in {
        val ols = new OnlineLogStatistics {}
        val observations = List(-3.7, 2.1, 0.8, 1.2, 0.8, -0.5).map(_ - logOffset)
        observations.foreach(ols.record)

        val logStats = ols.totalLogStatistics.multiplyByConstant(logOffset)
        logStats.logMean should be (1.0158 +- 1E-4)
        logStats.logVariance should be (2.1337 +- 1E-4)
        logStats.count should be (observations.length)
      }

      "exponentiation could overflow" in {
        val ols = new OnlineLogStatistics {}
        val observations = List(-3.7, 2.1, 0.8, 1.2, 0.8, -0.5).map(_ + logOffset)
        observations.foreach(ols.record)

        val logStats = ols.totalLogStatistics.multiplyByConstant(-logOffset)
        logStats.logMean should be (1.0158 +- 1E-4)
        logStats.logVariance should be (2.1337 +- 1E-4)
        logStats.count should be (observations.length)
      }
    }
  }
}
