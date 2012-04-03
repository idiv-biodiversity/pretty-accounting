package grid

import org.jfree.chart.ChartUtilities._

trait AccountingApp extends App with Accounting {
}

object EfficiencyByUser extends AccountingApp {
  (efficiencyGroupedBy(jobs) { _.user.uid } toList) sortBy { _._3 } map formatted foreach println
}

object EfficiencyByGroup extends AccountingApp {
  (efficiencyGroupedBy(jobs) { _.user.gid } toList) sortBy { _._3 } map formatted foreach println
}

trait ChartingApp extends AccountingApp {
  /** Returns the name that is used both for the title of the chart and the output file name. */
  def name: String

  /** Returns the regex used to parse width and height. */
  protected lazy val geometry = """(\d+)x(\d+)""".r

  lazy val (width,height) = sys.props get "chart.geometry" collect {
    case geometry(w,h) => w.toInt -> h.toInt
  } getOrElse 1920 -> 1080

  lazy val output = sys.props get "output.dir" map { dir =>
    if (dir endsWith fileSeparator) dir substring (0, dir.length - 1) else dir
  } filter {
    _.isDirectory
  } getOrElse {
    sys.env("HOME")
  } + "%s%s-%s.png" format (fileSeparator, name, DateTime.now)
}

object SlotsPerQueue extends ChartingApp {
  override lazy val name = "Slots per Queue"

  val dataset = jobs filter wasRunning groupBy { _.queue.get } perMinute { _.slots }
  val chart   = createTimeSeriesStackedAreaChart(dataset, name)

  saveChartAsPNG(output, chart, width, height)
}

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "Slots - Sequential vs. Parallel"

  val dataset = jobs filter wasRunning groupBy { j =>
    if (parallel(j)) "parallel" else "sequential"
  } perMinute { _.slots }

  val chart = createTimeSeriesStackedAreaChart(dataset, name)

  saveChartAsPNG(output, chart, width, height)
}
