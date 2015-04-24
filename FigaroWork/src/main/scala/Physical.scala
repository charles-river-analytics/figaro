import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous._
import com.cra.figaro.algorithm.sampling.Importance

object Physical {
  val inductance = 2.51
  val terminalResistance = 2.96
  val motorConstant = 0.21
  val voltage = Normal(0, 0.5)
  val current = Normal(45, 4)
  val angularVelocity = Apply(Uniform(0, 2 * math.Pi), (d: Double) => math.sin(d))
  val dCurrentDT =
    Apply(voltage, current, angularVelocity,
          (v: Double, c: Double, aV: Double) =>
            (1.0 / inductance) * (v - terminalResistance * c - motorConstant * aV))

  def main(args: Array[String]) {
    val alg = Importance(dCurrentDT)
    alg.start()
    Thread.sleep(1000)
    println(alg.expectation(dCurrentDT, (x: Double) => x))
    alg.kill()
  }
}
