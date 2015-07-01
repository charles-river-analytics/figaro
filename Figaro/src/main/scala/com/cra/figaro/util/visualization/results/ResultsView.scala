/**
 *
 */
package com.cra.figaro.util.visualization.results

import scala.collection.JavaConversions._
import prefuse.data.Table
import prefuse.data.Tuple
import prefuse.data.io.CSVTableReader
import prefuse.data.query.ObjectRangeModel
import scala.collection.JavaConversions

import com.cra.figaro.util.visualization.histogram.DataView

/**
 * @author Glenn Takata
 *
 * Mar 16, 2015
 */
class ResultsView[T](data: ResultsData[T]) extends DataView {
  val name = data.name
  val title = name
  
  def nValues = data.distribution.size
  
  def range: ObjectRangeModel = {
    val values = JavaConversions.asJavaCollection(data.distribution.map(_._2)).toArray()
    val range = new ObjectRangeModel(values.asInstanceOf[Array[Object]])
    range
  }
 
  def getTable = readTable
  
  def readTable: Table = {
      val resultsTable = new Table()
      resultsTable.addColumn("Name", classOf[String])
      resultsTable.addColumn("Value", classOf[Object])
      resultsTable.addColumn("Probability", classOf[Float])
      
      resultsTable.addRows(data.distribution.size)
      
      var row = 0
      val name = data.name
      for (value <- data.distribution) {        
   
        resultsTable.set(row, "Name", name)
        resultsTable.set(row, "Value", value._2)
        resultsTable.set(row, "Probability", value._1)
        
        row += 1
      }
      resultsTable

  }
}

class NoResult() extends ResultsView(ResultsData("No Data", List((1.0, true))))