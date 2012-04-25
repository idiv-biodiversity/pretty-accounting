package grid

import org.jfree.chart.ChartUtilities._

object SlotsPerQueue extends ChartingApp {
  override lazy val name = "Slots per Queue"

  val dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots }
  val chart   = createTimeSeriesStackedAreaChart(dataset, name)

  chart saveAs extension
}
