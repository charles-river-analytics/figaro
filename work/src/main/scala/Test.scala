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