package grid

import org.jfree.chart.ChartUtilities._

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "Slots - Sequential vs. Parallel"

  val dataset = dispatched groupBy { j =>
    if (parallel(j)) "parallel" else "sequential"
  } toTimeslots { _.slots }

  val chart = createTimeSeriesStackedAreaChart(dataset, name)

  chart saveAs extension
}
