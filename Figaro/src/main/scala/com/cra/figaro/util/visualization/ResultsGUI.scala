package com.cra.figaro.util.visualization

import scala.swing._
import com.cra.figaro.util.visualization.results.{ ResultsTable, ResultsView }
import com.cra.figaro.util.visualization.histogram.{ Histogram }
import com.cra.figaro.util.ColorGradient
import scala.swing.event.Event
import scala.swing.event.TableRowsSelected
import com.cra.figaro.util.visualization.results.ResultsData

/**
 * @author Glenn Takata (gtakata@cra.com)
 */

case class NewResult(result: ResultsData[_]) extends Event

class ResultHandler extends Publisher {
  def newResult(result: ResultsData[_]) {
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
  val TAB_WIDTH = 400
  val TAB_HEIGHT = 300
  
  val TABLE_WIDTH = 400
  val TABLE_HEIGHT = 250
  
  val results = new ResultHandler
  def addResult(result: ResultsData[_]) {
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
        println(row)
        updateHistogram(row)
        mainPanel.revalidate()
        mainPanel.repaint
      }
    }

    private def updateHistogram(result: ResultsData[_]) {
      graphs.pages.clear()

      val histogram = new Histogram(new ResultsView(result), currentColor)
      graphs.pages += new TabbedPane.Page(result.name, histogram)
      graphs.revalidate
      graphs.repaint
    }

    visible = true
  }
}
