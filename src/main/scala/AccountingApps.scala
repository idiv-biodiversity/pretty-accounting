package grid

import org.jfree.chart.ChartUtilities._
import scalaz.Scalaz._

trait AccountingApp extends App with Accounting {
  lazy val start = sys.props get "chart.start" flatMap { _ toDateTimeOption } orElse {
    Some(0L.toDateTime)
  }

  lazy val end   = sys.props get "chart.end"   flatMap { _ toDateTimeOption } orElse {
    Some(DateTime.now)
  }

  implicit lazy val interval = (start |@| end) {
    _ to _
  }
}

trait EfficiencyApp extends AccountingApp {
  def formatted(t: Tuple4[String,Int,Double,Double]) =
    "%10s -> %10d jobs -> %6.2f%% u -> %6.2f%% u+s" format (
      t._1,                             //  group
      t._2,                             //  jobs
      (t._3 * 10000).round / 100.0,     //  utime
      (t._4 * 10000).round / 100.0      //  utime + stime
    )
}

object EfficiencyByUser extends EfficiencyApp {
  (jobs groupBy { _.user.uid } efficiency).toList sortBy { _._3 } map formatted foreach println
}

object EfficiencyByGroup extends EfficiencyApp {
  (jobs groupBy { _.user.gid } efficiency).toList sortBy { _._3 } map formatted foreach println
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

  val dataset = jobs filter wasRunning groupBy { _.queue.get } toTimeslots { _.slots }
  val chart   = createTimeSeriesStackedAreaChart(dataset, name)

  saveChartAsPNG(output, chart, width, height)
}

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "Slots - Sequential vs. Parallel"

  val dataset = jobs filter wasRunning groupBy { j =>
    if (parallel(j)) "parallel" else "sequential"
  } toTimeslots { _.slots }

  val chart = createTimeSeriesStackedAreaChart(dataset, name)

  saveChartAsPNG(output, chart, width, height)
}
