package grid

import scalaz.Scalaz._

object AccountingApp extends Accounting {
  object IntervalExtractor {
    val intervalRE = """(\d{4}-\d{2}-\d{2})/(\d{4}-\d{2}-\d{2})""".r

    def unapply(s: String): Option[Interval] = s match {
      case intervalRE(from,to) => (from.toDateTimeOption |@| to.toDateTimeOption) { _ to _ }
      case _ => None
    }
  }
}

trait AccountingApp extends App with Accounting {
  /** Returns the name of this app. */
  def name: String

  /** Returns the default output file extension. */
  def defaultExtension: String

  implicit lazy val interval: Option[Interval] = sys.props get "grid.accounting.interval" map {
    _.trim.toLowerCase
  } collect {
    case "week"    => val now = DateTime.now ; (now - 1.week)   to now
    case "month"   => val now = DateTime.now ; (now - 1.months) to now
    case "quarter" => val now = DateTime.now ; (now - 3.months) to now
    case "year"    => val now = DateTime.now ; (now - 1.year)   to now
    case AccountingApp.IntervalExtractor(interval) => interval
  } orElse {
    val start = sys.props get "grid.accounting.start" flatMap { _ toDateTimeOption }
    val end   = sys.props get "grid.accounting.end"   flatMap { _ toDateTimeOption }

    (start |@| end) { _ to _ }
  }

  lazy val extension: String = sys.props get "grid.accounting.output.extension" getOrElse {
    defaultExtension
  }

  lazy val outputPath: String = sys.props get "grid.accounting.output.path" map { dir =>
    if (dir endsWith fileSeparator) dir substring (0, dir.length - 1) else dir
  } filter {
    _.isDirectory
  } getOrElse {
    sys.env("HOME")
  }

  implicit lazy val output: java.io.File = "%s%s%s.%s" format (
    outputPath,
    fileSeparator,
    sys.props get "grid.accounting.output.name" getOrElse {
      name + ("-" + (interval map { _.toString.replaceAll(fileSeparator,"-") } getOrElse { DateTime.now }))
    },
    extension
  )
}

trait EfficiencyApp extends AccountingApp {
  def defaultExtension = "txt"

  def formatted(t: Tuple4[String,Int,Double,Double]) =
    "%10s -> %10d jobs -> %6.2f%% u -> %6.2f%% u+s" format (
      t._1,                             //  group
      t._2,                             //  jobs
      (t._3 * 10000).round / 100.0,     //  utime
      (t._4 * 10000).round / 100.0      //  utime + stime
    )
}

trait ChartingApp extends AccountingApp {
  def defaultExtension = "png"

  /** Returns the regex used to parse width and height. */
  protected lazy val geometry = """(\d+)x(\d+)""".r

  implicit lazy val dim = sys.props get "grid.accounting.chart.geometry" collect {
    case geometry(w,h) => w.toInt -> h.toInt
  } getOrElse 1920 -> 1080
}
