package grid

import org.jfree.chart.ChartFactory._
import org.jfree.chart.plot.PlotOrientation._

object RichCharting extends RichCharting

trait RichCharting extends TypeImports with StaticImports {
  implicit def joda2jfreeminute(d: DateTime) = new org.jfree.data.time.Minute(d.toDate)

  def show(chart: JFreeChart)(implicit title: String = "") = onEDT {
    new ChartFrame(title, chart, true) setVisible true
  }

  def createTimeSeriesAreaChart(implicit dataset: XYDataset, title: String = "") = {
    val chart = createXYAreaChart (
      /* title       = */ title,
      /* xAxisLabel  = */ "",
      /* yAxisLabel  = */ "",
      /* dataset     = */ dataset,
      /* orientation = */ VERTICAL,
      /* legend      = */ true,
      /* tooltips    = */ false,
      /* urls        = */ false
    )
    chart.getXYPlot.setDomainAxis(new DateAxis)
    chart
  }

  def createTimeSeriesStackedAreaChart(implicit dataset: TableXYDataset, title: String = "") = {
    val chart = createStackedXYAreaChart (
      /* title       = */ title,
      /* xAxisLabel  = */ "",
      /* yAxisLabel  = */ "",
      /* dataset     = */ dataset,
      /* orientation = */ VERTICAL,
      /* legend      = */ true,
      /* tooltips    = */ false,
      /* urls        = */ false
    )
    chart.getXYPlot.setDomainAxis(new DateAxis)
    chart
  }
}
