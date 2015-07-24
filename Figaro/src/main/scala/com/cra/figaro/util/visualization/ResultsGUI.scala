/*
 * ResultsGUI.scala 
 * The main controller for visualizations. Coordinates data input and display as well as user interaction with displays.
 * 
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Mar 16, 2015
 * 
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.util.visualization

import scala.swing._
import com.cra.figaro.language.{Element}
import com.cra.figaro.util.visualization.results.{ ContinuousData, DiscreteData, ResultsData, ResultsTable, ResultsView }
import com.cra.figaro.util.visualization.histogram.{ Histogram }
import com.cra.figaro.util.visualization.distribution.{Distribution}
import com.cra.figaro.util.visualization.reduction.DataReduction
import com.cra.figaro.util.ColorGradient
import scala.swing.event.Event
import scala.swing.event.TableRowsSelected

/**
 * @author Glenn Takata (gtakata@cra.com)
 */

case class NewResult(result: ResultsData) extends Event

class ResultHandler extends Publisher {
  def newResult(result: ResultsData) {
    Swing.onEDT(
      publish(NewResult(result))
    )
  }
}

class EmptyTab extends BoxPanel(Orientation.Vertical) {
  import ResultsGUI._
   preferredSize = new Dimension(TAB_WIDTH, TAB_HEIGHT)
}

object ResultsGUI extends SimpleSwingApplication {
  val TAB_WIDTH = 600
  val TAB_HEIGHT = 300
  
  val TABLE_WIDTH = 600
  val TABLE_HEIGHT = 250
  
  val results = new ResultHandler
//  def addResult(result: ResultsData) {
  def addResult(name: String, dist: Any) {
    val result = dist match {
      case l: List[(Double, Double)] => DiscreteData(name, DataReduction.binToDistribution(l)) 
      case e: Element[_] => ContinuousData(name, e)
    }
    results.newResult(result)
  }

  def top = new MainFrame {
    title = "Figaro Results"

    preferredSize = new Dimension(TAB_WIDTH, 600)

    var currentColor: String = ColorGradient.SINGLECOLOR

    def setColor(color: String) {
      currentColor = color
      table.revalidate
    }

    // table 
    val table = new ResultsTable

    val graphs = new TabbedPane() {
      preferredSize = new Dimension(TAB_WIDTH, TAB_HEIGHT)
      resizable = true
      
      pages += new TabbedPane.Page("", new EmptyTab)
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }

    val mainPanel = new BorderPanel {
      import BorderPanel.Position._
      resizable = true
      add(graphs, Center)
      add(table, South)
    }

    contents = mainPanel
    
    listenTo(results, table.getSelection)
    reactions += {
      case NewResult(result) => {
        table.add(result)
        // histogram
        updateHistogram(result)
        mainPanel.revalidate()
        mainPanel.repaint
      }
      case TableRowsSelected(source, range, false) => {
        val row = table.getSelectedRow
//        println(row)
        updateHistogram(row)
        mainPanel.revalidate()
        mainPanel.repaint
      }
    }

    private def updateHistogram(result: ResultsData) {
      graphs.pages.clear()
      
      result match {
        case DiscreteData(name, dist) => {
            val color = currentColor
            val histogramTab = new Histogram(new ResultsView(result), color)
            graphs.pages += new TabbedPane.Page(result.name + " Distribution", histogramTab)
        }
        case ContinuousData(name, dist) => { 
          val color = ColorGradient.HEATMAP
          val distributionTab = new Distribution(new ResultsView(result), color)
          graphs.pages += new TabbedPane.Page(result.name + " Density", distributionTab)

        }
        case _ =>
      }
      
      graphs.revalidate
      graphs.repaint
    }

    visible = true
  }
}
