import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.If

object BPTest {
  def main(args: Array[String]) {
    val e1 = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
    val e2 = Flip(e1)
    val e3 = Apply(e2, (b: Boolean) => b)
    e3.observe(true)

    println(BeliefPropagation.probability(e1, 0.3))
    val alg = BeliefPropagation(e1)
    alg.start()
    println(VariableElimination.probability(e1, 0.3))
  }
}
