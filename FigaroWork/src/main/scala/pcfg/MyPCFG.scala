package pcfg

import PCFG._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.experimental.structured.algorithm.StructuredVE

object MyPCFG {
  val p = Terminal(0.4 -> 'a', 0.6 -> 'b')
  val q = Terminal(0.1 -> 'b', 0.9 -> 'c')
  val r: Nonterminal = Nonterminal(0.1 -> (r _, r _), 0.1 -> (p _, r _), 0.2 -> (p _, p _), 0.3 -> (p _, q _), 0.1 -> (q _, r _), 0.2 -> (q _, q _))

  val max = 10

  def main(args: Array[String]) {
    for { i <- 2 to 2 } {
      val len2 = i/3
      val len1 = i - len2 - len2
      val evidence = "a" * len1 + "b" * len2 + "c" * len2
      val myCheck = Checker(evidence).check(r)
      val time0 = System.currentTimeMillis()
//    val prob = VariableElimination.probability(myCheck, true)
      val prob = StructuredVE.probability(myCheck, true)
      val time1 = System.currentTimeMillis()
      println("Length " + i + ": time = " + ((time1 - time0) / 1000.0) + ", probability = " + prob)
    }
  }
}
