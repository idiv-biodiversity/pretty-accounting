package grid

import org.jfree.chart.ChartUtilities._

trait AccountingApp extends App with Accounting {
  def output(name: String)(implicit path: String = sys.env("HOME")) =
    "%s%s%s-%s.png" format (path, fileSeparator, name, DateTime.now)
}

object SlotsPerQueue extends AccountingApp {
  val dataset = (jobs filter wasRunning groupBy { _.queue.get }).perMinute { _.slots }
  val chart   = createTimeSeriesStackedAreaChart(dataset, "Slots per Queue")
  saveChartAsPNG(output("slots-per-queue"), chart, 1920, 1080)
}

object SlotsSequentialVsParallel extends AccountingApp {
  val dataset = (jobs filter wasRunning groupBy { j =>
    if (parallel(j)) "parallel" else "sequential"
  }).perMinute { _.slots }
  val chart   = createTimeSeriesStackedAreaChart(dataset, "Slots - Sequential vs. Parallel")
  saveChartAsPNG(output("slots-sequential-vs-parallel"), chart, 1920, 1080)
}

object EfficiencyByUser extends AccountingApp {
  (efficiencyGroupedBy(jobs) { _.user.uid } toList) sortBy { _._3 } map formatted foreach println
}

object EfficiencyByGroup extends AccountingApp {
  (efficiencyGroupedBy(jobs) { _.user.gid } toList) sortBy { _._3 } map formatted foreach println
}
