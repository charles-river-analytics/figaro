/*
 * ResultsTable.scala 
 * Visual element for a table to display user inputs. Includes discrete (distribution List) and continuous (element)
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Apr 9, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization.results

import java.awt.Dimension
import javax.swing.table.{ AbstractTableModel }
import scala.collection._
import scala.swing.{ BoxPanel, Orientation, ScrollPane, Table }
import com.cra.figaro.util.visualization.{ResultsGUI}

/**
 * @author gtakata
 */
class ResultsTable extends BoxPanel(Orientation.Vertical) {
  import ResultsGUI._
  
  preferredSize = new Dimension(TAB_WIDTH, TABLE_HEIGHT)
  
  def add(result: ResultsData) {
    tableModel.addResult(result)
    
    table.revalidate
    table.repaint
  }

  val tableModel = new ResultsTableModel(Array[Array[Any]](), List("Element", "Distribution"))
  val table = new Table() {
    model = tableModel
  }

  def getSelectedRow = {
    val row = table.selection.rows.head
    tableModel.getRow(row)
  }
  
  def getSelection = {
    table.selection  
  }
  
  contents += new ScrollPane(table)
}

class ResultsTableModel(var rowData: Array[Array[Any]], val columnNames: Seq[String]) extends AbstractTableModel {
  override def getColumnName(column: Int) = columnNames(column).toString()

  var results = new mutable.ListBuffer[ResultsData]()

  def getRowCount() = rowData.length
  def getColumnCount() = columnNames.length

  def getValueAt(row: Int, col: Int): AnyRef = {
    val result = results(row)
    col match {
      case 0 => result.name
      case 1 => result.resultString
      case _ => ""
    }
  }

  override def isCellEditable(row: Int, column: Int) = false
  override def setValueAt(value: Any, row: Int, col: Int) {
    rowData(row)(col) = value
  }

  def addRow( data: Array[AnyRef]) {
    rowData ++= Array(data.asInstanceOf[Array[Any]])
  }
  
  def addResult(result: ResultsData) {
    results.append(result)
    addRow(Array[AnyRef](result.name, result.distribution))
  }

  def getRow(row: Int) = {
    results(row)
  }
}

