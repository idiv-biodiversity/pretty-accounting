package grid

import scalaz.Scalaz._

trait AccountingApp extends App with Accounting {
  def dispatched = jobs filter isDispatched

  lazy val start = sys.props get "chart.start" flatMap { _ toDateTimeOption }
  lazy val end   = sys.props get "chart.end"   flatMap { _ toDateTimeOption }

  implicit lazy val interval = (start |@| end) { _ to _ }
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

trait ChartingApp extends AccountingApp {
  /** Returns the name that is used both for the title of the chart and the output file name. */
  def name: String

  /** Returns the regex used to parse width and height. */
  protected lazy val geometry = """(\d+)x(\d+)""".r

  lazy val (width,height) = sys.props get "chart.geometry" collect {
    case geometry(w,h) => w.toInt -> h.toInt
  } getOrElse 1920 -> 1080

  lazy val output: String = sys.props get "output.path" getOrElse {
    sys.props get "output.dir" map { dir =>
      if (dir endsWith fileSeparator) dir substring (0, dir.length - 1) else dir
    } filter {
      _.isDirectory
    } getOrElse {
      sys.env("HOME")
    } + "%s%s-%s-%dx%d.png" format (
      fileSeparator,
      name,
      interval map { _.toString.replaceAll(fileSeparator,"-") } getOrElse { DateTime.now },
      width,
      height
    )
  }
}
