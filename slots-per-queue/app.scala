package grid

import org.jfree.chart.ChartUtilities._

object SlotsPerQueue extends ChartingApp {
  override lazy val name = "slots-per-queue"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots }
  ) saveAs extension
}
