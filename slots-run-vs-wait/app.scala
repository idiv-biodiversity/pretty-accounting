package grid

import org.jfree.chart.ChartUtilities._

object SlotsRunVsWait extends ChartingApp {
  override lazy val name = "Slots - Running vs. Waiting"

  val dataset = dispatched toPendingVsRunning

  createTimeSeriesStackedAreaChart(dataset, name) saveAs extension
}
