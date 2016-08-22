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

package com.cra.figaro.test.experimental.marginalmap

import com.cra.figaro.experimental.marginalmap.LogStatistics
import com.cra.figaro.experimental.marginalmap.LogStatistics.oneSidedTTest
import org.scalatest.{Matchers, WordSpec}

class LogStatisticsTest extends WordSpec with Matchers {
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
        val v1 = LogStatistics(0.0, 0.0, 20).multiplyByConstant(-100000)
        val v2 = LogStatistics(0.1, 0.1, 30).multiplyByConstant(-100000)

        oneSidedTTest(v1, v2) should be (0.36147 +- 1E-5)
      }

      "exponentiation could overflow" in {
        val v1 = LogStatistics(0.0, 0.0, 20).multiplyByConstant(100000)
        val v2 = LogStatistics(0.1, 0.1, 30).multiplyByConstant(100000)

        oneSidedTTest(v1, v2) should be (0.36147 +- 1E-5)
      }
    }
  }
}
