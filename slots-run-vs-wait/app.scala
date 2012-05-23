package grid

import org.jfree.chart.ChartUtilities._

object SlotsRunVsWait extends ChartingApp {
  override lazy val name = "slots-run-vs-wait"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = raw filter realJob filter isDispatched toPendingVsRunning
  ) saveAs extension
}
