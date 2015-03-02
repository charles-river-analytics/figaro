/*
 * Test.scala 
 * A simple Figaro test program.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com)
 * Creation Date:   Aug 6, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._

object Test {
	def main(args: Array[String]) {
		val test = Constant("Test")

		val algorithm = Importance(1000, test)
		algorithm.start()
		
		println(algorithm.probability(test, "Test"))
	}
}