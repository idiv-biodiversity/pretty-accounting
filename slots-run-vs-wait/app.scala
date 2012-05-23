package grid

import org.jfree.chart.ChartUtilities._

object SlotsRunVsWait extends ChartingApp {
  override lazy val name = "slots-run-vs-wait"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = dispatched toPendingVsRunning
  ) saveAs extension
}
