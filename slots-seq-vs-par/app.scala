package grid

import org.jfree.chart.ChartUtilities._

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "slots-seq-vs-par"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy {
      j => if (parallel(j)) "parallel".localized else "sequential".localized
    } toTimeslots {
      _.slots
    }
  ) saveAs extension
}
