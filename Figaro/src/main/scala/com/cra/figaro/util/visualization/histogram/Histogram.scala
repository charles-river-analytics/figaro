package com.cra.figaro.util.visualization.histogram

import java.awt.Color
import java.awt.event._
import java.awt.geom.Rectangle2D
import java.text.NumberFormat
import javax.swing.BorderFactory
import javax.swing.Box

import scala.collection.JavaConversions
import scala.swing._
import scala.swing.BorderPanel.Position._

import prefuse.Constants
import prefuse.Display
import prefuse.Visualization
import prefuse.action.ActionList
import prefuse.action.RepaintAction
import prefuse.action.assignment.ColorAction
import prefuse.action.filter.VisibilityFilter
import prefuse.action.layout.AxisLayout
import prefuse.action.layout.AxisLabelLayout
import prefuse.controls.ControlAdapter
import prefuse.data.Table
import prefuse.data.expression.AndPredicate
import prefuse.data.expression.Expression
import prefuse.data.expression.Predicate
import prefuse.data.expression.parser.ExpressionParser
import prefuse.data.io.CSVTableReader
import prefuse.data.query.NumberRangeModel
import prefuse.data.query.RangeQueryBinding
import prefuse.render.RendererFactory
import prefuse.render.AxisRenderer
import prefuse.util.ColorLib
import prefuse.util.FontLib
import prefuse.util.UpdateListener
import prefuse.util.ui.JFastLabel
import prefuse.util.ui.UILib
import prefuse.visual.VisualItem
import prefuse.visual.expression.VisiblePredicate

import com.cra.figaro.util.visualization.ResultsGUI._

/**
 * @author Glenn Takata (gtakata@cra.com)
 */
class Histogram(val dataview: DataView, var color: String) extends BorderPanel {
  // fonts, colours, etc.
  UILib.setColor(peer, ColorLib.getColor(0, 0, 0), Color.BLACK);
  val itemRenderer = new HistogramRenderer(color, dataview.nValues)

  // title
  val title = new Label(dataview.title)
  title.preferredSize = new Dimension(200, 20)
  title.verticalAlignment = Alignment.Top
  title.font = FontLib.getFont("Tahoma", 18)

  // visualization (main container)
  val vis: Visualization = new Visualization()
  val visualTable = vis.addTable(dataview.name, dataview.getTable)

  // dynamic query based on name
  val valueQ: RangeQueryBinding = new RangeQueryBinding(visualTable, "Value");
  val filter: AndPredicate = new AndPredicate(valueQ.getPredicate());

  // X-axis
  val xaxis: AxisLayout = new AxisLayout(dataview.name, "Value", Constants.X_AXIS, VisiblePredicate.TRUE);

  // ensure the axis spans the width of the data container
  xaxis.setDataType(Constants.NOMINAL)
  xaxis.setRangeModel(dataview.range)

  // add the labels to the x-axis
  val xlabels: AxisLabelLayout = new AxisLabelLayout("xlab", xaxis);
  vis.putAction("xlabels", xlabels)

  // Y-axis
  val yaxis: AxisLayout = new AxisLayout(dataview.name, "Probability", Constants.Y_AXIS, VisiblePredicate.TRUE);

  // ensure the y-axis spans the height of the data container

  // set the y-axis range
  val rangeModel = new NumberRangeModel(0, 1.0, 0, 1.0);
  yaxis.setRangeModel(rangeModel)
  // add the labels to the y-axis
  val ylabels: AxisLabelLayout = new AxisLabelLayout("ylab", yaxis);
  val nf = NumberFormat.getIntegerInstance();
  nf.setMaximumFractionDigits(2);
  ylabels.setNumberFormat(nf);

  // drawing actions
  // specify the fill (interior) as a static colour (white)
  val fill: ColorAction = new ColorAction(dataview.name, VisualItem.FILLCOLOR, 0);

  val draw: ActionList = new ActionList()
  draw.add(fill)
  draw.add(xaxis)
  draw.add(yaxis)
  draw.add(ylabels)
  draw.add(new RepaintAction())
  vis.putAction("draw", draw)

  // update actions
  val update: ActionList = new ActionList()
  update.add(new VisibilityFilter(dataview.name, filter)); // filter performs the size/name filtering
  update.add(xaxis)
  update.add(yaxis)
  update.add(ylabels)
  update.add(new RepaintAction())
  vis.putAction("update", update)

  // create an update listener that will update the visualization when fired
  val lstnr: UpdateListener = new UpdateListener() {
    def update(src: Object) {
      vis.run("update");
    }
  };

  // add this update listener to the filter, so that when the filter changes (i.e.,
  // the user adjusts the axis parameters, or enters a name for filtering), the 
  // visualization is updated
  filter.addExpressionListener(lstnr);

  // add the listener to this component
  peer.addComponentListener(lstnr);


  val display = setupDisplay(vis)

  vis.setRendererFactory(new RendererFactory() {

    val yAxisRenderer = new AxisRenderer(Constants.FAR_LEFT, Constants.CENTER)
    val xAxisRenderer = new AxisRenderer(Constants.CENTER, Constants.BOTTOM)

    def getRenderer(item: VisualItem) = {
      if (item.isInGroup("ylab")) {
        yAxisRenderer
      } else if (item.isInGroup("xlab")) {
        xAxisRenderer
      } else {
        itemRenderer
      }
    }
  })

  // container for elements at the top of the screen
  val topContainer = new BoxPanel(Orientation.Horizontal) {
    peer.add(Box.createHorizontalStrut(5));
    contents += title
    peer.add(Box.createHorizontalGlue());
    peer.add(Box.createHorizontalStrut(5));
  }

  // add the containers to the JPanel
  layout(topContainer) = North
  layout(Component.wrap(display)) = Center
  //  add(slider, BorderLayout.SOUTH);

  vis.run("draw");
  vis.run("xlabels");

  def setupDisplay(visualization: Visualization) = {
    val disp = new Display(visualization);

    // set the display properties
    disp.setBorder(BorderFactory.createEmptyBorder(5, 20, 5, 10));
    disp.setSize(new Dimension(TAB_WIDTH, TAB_HEIGHT));
    disp.setHighQuality(true);

    // call the function that sets the sizes of the containers that contain
    // the data and the axes
    displayLayout(disp, dataview);

    // whenever the window is re-sized, update the layout of the axes
    disp.addComponentListener(new ComponentAdapter() {

      override def componentResized(e: ComponentEvent) {
        displayLayout(disp, dataview);
      }
    });

    disp.addControlListener(new ControlAdapter() {
      override def itemClicked(item: VisualItem, event: MouseEvent) {
        val table = dataview.getTable
        val filter = ExpressionParser.predicate("Name = " + item.getSourceTuple.get("Name"))
        val rows = table.rows(filter)
        for (item <- JavaConversions.asScalaIterator(table.tuples(rows))) {
          println(item)
        }
      }
    });

    disp
  }

  /*
     * calculate the sizes of the data and axes containers based on the
     * display size, and then tell the visualization to update itself and
     * re-draw the x-axis labels
     */
  def displayLayout(display: Display, data: DataView) {
    val insets = display.getInsets();
    val width = display.getWidth();
    val height = display.getHeight();
    val insetWidth = insets.left + insets.right;
    val insetHeight = insets.top + insets.bottom;

    val viewXOffset = 20
    val viewYOffset = 10
    val yAxisWidth = 5;
    val xAxisHeight = 10;
    val displayHeight = height - xAxisHeight - insetHeight - 2 * viewYOffset
    val maxDisplayWidth = width - yAxisWidth - insetWidth - 2 * viewXOffset
    
    val displayWidth = math.min(data.range.getExtent * 60, maxDisplayWidth)
    
    val dataView: Rectangle2D = new Rectangle2D.Double(insets.left + yAxisWidth + viewXOffset, insets.top + viewYOffset, displayWidth, displayHeight)
    val xView: Rectangle2D = new Rectangle2D.Double(insets.left + yAxisWidth + viewXOffset, insets.top + displayHeight + viewYOffset , displayWidth, xAxisHeight)
    val yView: Rectangle2D = new Rectangle2D.Double(insets.left, insets.top + viewYOffset, yAxisWidth, displayHeight)

    // reset all the bounds
    itemRenderer.setBounds(dataView)

    xaxis.setLayoutBounds(dataView);
    xlabels.setLayoutBounds(xView)
    
    yaxis.setLayoutBounds(dataView);
    ylabels.setLayoutBounds(yView)

    vis.run("update");
    vis.run("xlabels");
  }
}