package com.cra.appril.birdmigration.data.io
import scala.io.Source
import scala.collection._
import com.cra.appril.birdmigration.data.CellFeatures

class DataReader(val gridDimension: Int) {

  val numberOfCells = gridDimension * gridDimension
  
  //These are the indices of the columns in the CSV files.
  val yearField = 0
  val dayField = 1
  //Note: Cells are indexed in Matlab order (column major).
  val birdsInCellFields: Seq[Int] = Seq.range(2, numberOfCells + 2)
  val fromCellField = 2
  val toCellField = 3
  val distanceField = 4
  val vectorDifferenceField = 5
  val windDirectionField = 6
  val fromEqualsToField = 7

  def stringToBoolean(s: String): Boolean = {
    if (s.equals("0")) {
      false
    }
    true
  }

  def readObservationsFile(fileName: String): immutable.Map[Int,Int] =
    {
	  val birdsInCells = mutable.Map.empty[Int,Int]
      val src = Source.fromFile(fileName);
      val iter = src.getLines().drop(1).map(_.split(","))
      //For each day in each year, there should be one of these records.
      while (iter.hasNext) {

        val row = iter.next
        val year = row(yearField).toInt
        val day = row(dayField).toInt
        for (cell <- birdsInCellFields) {
          val key = (year*10000) + (day*100) + (cell-1)
          if (birdsInCells.contains(key)) {
            throw new IllegalArgumentException
          }
          else {
     
              //println("y: " + year + " d: " + day + " c: " + cell + " k: " + key)
         
        	  birdsInCells.put(key, row(cell).toInt)
          }
        }

      }
      src.close()
      src.reader.close()
      src.reset()
      birdsInCells.toMap
     
    }

  def readFeaturesFile(fileName: String): immutable.Map[Int,CellFeatures] =
    {
	  val cellFeatures = mutable.Map.empty[Int,CellFeatures]
      val src = Source.fromFile(fileName);
      val iter = src.getLines().drop(1)
      //For each pair of cells on each day in each year, there should be dimension*dimension of these features.
      while (iter.hasNext) {
        val line = iter.next
        //println(line)
        val row = line.split(",")
        val year = row(yearField).toInt
        val day = row(dayField).toInt
        val fromCell = row(fromCellField).toInt
        val toCell = row(toCellField).toInt
        val distance = row(distanceField).toFloat
        val vectorDifference = row(vectorDifferenceField).toFloat
        val windDirection = row(windDirectionField).toFloat
        val fromEqualsTo = stringToBoolean(row(fromEqualsToField))

        val features = CellFeatures(distance, vectorDifference, windDirection, fromEqualsTo)
        

        val key = (year*1000000) + (day*10000)+(((fromCell<<16)^toCell))
        
        if (cellFeatures.contains(key)) {
            throw new IllegalArgumentException
        }
        else {
        	cellFeatures.put(key, features)
        }

      }
      src.close()
      cellFeatures.toMap
    }
}


object DataReader {
  def apply(size: Int) = new DataReader(size)
}