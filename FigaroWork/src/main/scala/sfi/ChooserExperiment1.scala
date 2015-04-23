package sfi

import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, FoldLeft}
import scala.util.Random._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.experimental.structured.algorithm.{StructuredVE, StructuredBP, StructuredVEBPChooser}

object ChooserExperiment1 {
  class Model(numDiseases: Int, numSymptoms: Int, numParentsPerSymptom: Int) {
    val universe = Universe.createNew()
    def f(b: Boolean): Element[Boolean] = {
      val w = if(b) Flip(0.9) else Flip(0.1)
      val x1 = Apply(w, (b: Boolean) => b)
      val y1 = Apply(x1, (b: Boolean) => b)
      val x2 = Apply(w, (b: Boolean) => b)
      val y2 = Apply(x2, (b: Boolean) => b)
      y1 || y2
    }

    def makeSymptom(diseases: Array[Element[Boolean]], numParents: Int): Element[Boolean] = {
      var unchosen = diseases.toSet
      var toDo = numParents
      var choices: List[Element[Boolean]] = List()
      while (toDo > 0) {
        val choiceId: Int = nextInt(unchosen.size)
        val ul: List[Element[Boolean]] = unchosen.toList
        val choice: Element[Boolean] = ul(choiceId)
        unchosen -= choice
        choices ::= choice
        toDo -= 1
      }
      val fs: List[Element[Boolean]] = choices.map(Chain(_, f _))
      val ij: Element[List[Boolean]] = Inject(fs:_*)
      Apply(ij, (l: List[Boolean]) => l.contains(true))
//      FoldLeft(false, (b1: Boolean, b2: Boolean) => b1 || b2)(fs:_*)
    }

    val topDiseases: Array[Element[Boolean]] = Array.tabulate(numDiseases)((i: Int) => Flip(0.1)("disease " + i, universe))
    val middleDiseases: Array[Element[Boolean]] = Array.tabulate(numDiseases)((i: Int) => makeSymptom(topDiseases, numParentsPerSymptom))
    val symptoms: Array[Element[Boolean]] = Array.tabulate(numSymptoms)((i: Int) => makeSymptom(middleDiseases, numParentsPerSymptom))
    for { symptom <- symptoms } {
      val evidence = nextDouble < 0.5
      if (nextDouble < 0.5) symptom.observe(evidence)
    }
    for { disease <- topDiseases } {
      val evidence = nextDouble < 0.5
      if (nextDouble < 0.5) disease.observe(evidence)
    }
  }

  def main(args: Array[String]) {
    val numRounds = 50
    val numParentsPerSymptom = 4
    val minDiseases = 4
    val maxDiseases = 10
    val diseaseRange = maxDiseases - minDiseases + 1
    val averageBPError = Array.fill(diseaseRange, numRounds)(0.0)
    val maxBPError = Array.fill(diseaseRange, numRounds)(0.0)
    val averageChooserError = Array.fill(diseaseRange, numRounds)(0.0)
    val maxChooserError = Array.fill(diseaseRange, numRounds)(0.0)
    val vETime = Array.fill(diseaseRange, numRounds)(0.0)
    val bPTime = Array.fill(diseaseRange, numRounds)(0.0)
    val chooserTime = Array.fill(diseaseRange, numRounds)(0.0)
    for { i <- 0 until numRounds } {
      for { j <- 0 until diseaseRange } {
        val numDiseases = j + minDiseases
        val numSymptoms = numDiseases
        val model = new Model(numDiseases, numSymptoms, numParentsPerSymptom)
        print("Round " + i + ": ")
        print("Number of diseases = " + numDiseases)
        print(", number of symptoms = " + numSymptoms)
        println(", number of parents per symptom = " + numParentsPerSymptom)

        val time0 = System.currentTimeMillis()
        val probsVE = (0 until numDiseases).map((i: Int) => StructuredVE.probability(model.middleDiseases(i), true))
        val time1 = System.currentTimeMillis()
        vETime(j)(i) = (time1 - time0) / 1000.0
        println("Variable elimination: time = " + vETime(j)(i))

        val time2 = System.currentTimeMillis()
        val probsBP = (0 until numDiseases).map((i: Int) => StructuredBP.probability(model.middleDiseases(i), true, 10))
        val time3 = System.currentTimeMillis()
        bPTime(j)(i) = (time3 - time2) / 1000.0
        val errorsBP = for { (prob1, prob2) <- probsVE.zip(probsBP) } yield scala.math.abs(prob2 - prob1)
        averageBPError(j)(i) = errorsBP.sum / numSymptoms
        maxBPError(j)(i) = errorsBP.max
        println("Belief propagation: time = " + bPTime(j)(i) + ", average error = " + averageBPError(j)(i) + ", max error = " + maxBPError(j)(i))

        val time4 = System.currentTimeMillis()
        val probsChooser = (0 until numDiseases).map((i: Int) => StructuredVEBPChooser.probability(model.middleDiseases(i), true, 0.0, 10))
        val time5 = System.currentTimeMillis()
        chooserTime(j)(i) = (time5 - time4) / 1000.0
        val errorsChooser = for { (prob1, prob2) <- probsVE.zip(probsChooser) } yield scala.math.abs(prob2 - prob1)
        averageChooserError(j)(i) = errorsChooser.sum / numSymptoms
        maxChooserError(j)(i) = errorsChooser.max
        println("VE/BP Chooser: time = " + chooserTime(j)(i) + ", average error = " + averageChooserError(j)(i) + ", max error = " + maxChooserError(j)(i))
      } // end loop over numDiseases

      val n = i + 1
      println("\nEnd of round " + n)
      println("=================================================================================================================================================")
      println("| n  | VE Avg Time | BP Avg Time | Ch Avg Time | VE Max Time | BP Max Time | Ch Max Time | BP Avg Err  | Ch Avg Err  | BP Max Err  | Ch Max Err |")
      for { j <- 0 until diseaseRange } {
        val vet = vETime(j).take(n)
        val veat = vet.sum / n
        val vemt = vet.max
        val bpt = bPTime(j).take(n)
        val bpat = bpt.sum / n
        val bpmt = bpt.max
        val cht = chooserTime(j).take(n)
        val chat = cht.sum / n
        val chmt = cht.max
        val bpae = averageBPError(j).take(n).sum / n
        val bpme = maxBPError(j).take(n).max
        val chae = averageChooserError(j).take(n).sum / n
        val chme = maxChooserError(j).take(n).max
        print("| " + (j + minDiseases).toDouble.toString.take(2) + " |    ")
        print(veat.toString.take(5) + "    |    " + bpat.toString.take(5) + "    |    " + chat.toString.take(5) + "    |    ")
        print(vemt.toString.take(5) + "    |    " + bpmt.toString.take(5) + "    |    " + chmt.toString.take(5) + "    |    ")
        println(bpae.toString.take(5) + "    |    " + chae.toString.take(5) + "    |    " + bpme.toString.take(5) + "    |    " + chme.toString.take(5) + "    |")
      }
      println("=================================================================================================================================================")
      println()
    }
  }
}
