package grid

import scalaz.Scalaz._

object AccountingApp extends Accounting {
  object IntervalExtractor {
    def unapply(s: String): Option[Interval] = s.toInterval.toOption
  }
}

trait AccountingApp extends App with Accounting {
  /** Returns the name of this app. */
  def name: String

  /** Returns the default output file extension. */
  def defaultExtension: String

  implicit def interval: Option[Interval] = sys.props get "grid.accounting.interval" map {
    _.trim.toLowerCase
  } collect {
    case "week"    ⇒ val now = DateTime.now ; (now - 1.week)   to now
    case "month"   ⇒ val now = DateTime.now ; (now - 1.months) to now
    case "quarter" ⇒ val now = DateTime.now ; (now - 3.months) to now
    case "year"    ⇒ val now = DateTime.now ; (now - 1.year)   to now
    case AccountingApp.IntervalExtractor(interval) ⇒ interval
  } orElse {
    sys.props get "grid.accounting.year" flatMap { y ⇒ y.toDateTime.toOption } map {
      year ⇒ year to (year + 1.year)
    }
  } orElse {
    val start = sys.props get "grid.accounting.start" flatMap { _.toDateTime.toOption }
    val end   = sys.props get "grid.accounting.end"   flatMap { _.toDateTime.toOption }

    (start ⊛ end) { _ to _ }
  }

  def extension: String = sys.props get "grid.accounting.output.extension" getOrElse {
    defaultExtension
  }

  def outputPath: String = sys.props get "grid.accounting.output.path" map { dir ⇒
    if (dir endsWith fileSeparator) dir substring (0, dir.length - 1) else dir
  } filter {
    _.isDirectory
  } getOrElse {
    sys.props get "java.io.tmpdir" getOrElse util.Properties.userHome
  }

  def output: java.io.File = "%s%s%s.%s" format (
    outputPath,
    fileSeparator,
    sys.props get "grid.accounting.output.name" getOrElse {
      name + ("-" + (interval map { _.toString.replaceAll(fileSeparator,"-") } getOrElse { DateTime.now }))
    },
    extension
  )
}
