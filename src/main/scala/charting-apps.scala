package grid

import scalax.chart.Chart

import scalaz.Monoid
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.map._
import scalaz.std.tuple._

object ChartingApp {
  /** Returns the regex used to parse width and height. */
  def geometry = """(\d+)x(\d+)""".r

  /** Partitions the map in a high and a low part and sums up the low part into "others". */
  def lowToOthers(kvs: Map[String,Double], lowPercent: Double): Iterable[(String,Double)] = {
    val sum = kvs.foldLeft(0.0)(_ + _._2)
    val (high,low) = kvs.partition(_._2 / sum > lowPercent)
    val others = "others".localized -> low.foldLeft(0.0)(_ + _._2)
    others :: high.sortBy(_._2)
  }

  implicit def DoubleMonoid: Monoid[Double] = new Monoid[Double] {
    def zero = 0.0
    def append(a: Double, b: => Double) = a + b
  }

  def inverseCategories[A,B,C](m: Map[A,Iterable[(B,C)]]): Map[B,Map[A,C]] = {
    m.foldLeft(Map.empty[B,Map[A,C]]) {
      case (n,(a,bc)) =>
        bc.foldLeft(n) {
          case (n,(b,c)) =>
            if (n contains b) {
              n.updated(b,n(b) + ((a,c)))
            } else {
              n + (b -> Map(a -> c))
            }
        }
    }
  }
}

trait ChartingApp extends AccountingApp {
  /** Returns the chart that will be saved. */
  def chart: Chart

  def defaultExtension = "png"

  def lowPercent = sys.props.getOrElse("grid.accounting.chart.pie.exclude", "0.01").toDouble

  def dim = sys.props get "grid.accounting.chart.geometry" collect {
    case ChartingApp.geometry(w,h) ⇒ w.toInt → h.toInt
  } getOrElse 1920 → 1080

  extension.toLowerCase match {
    // case "pdf" ⇒ chart.saveAsPDF(output, dim)
    case "png" ⇒ chart.saveAsPNG(output, dim)
    case "jpg" | "jpeg" ⇒ chart.saveAsJPEG(output, dim)
  }
}

object CPUTimePerDepartment extends ChartingApp {
  def name = "cputime-per-department"

  def data = {
    import ChartingApp.DoubleMonoid

    val cxs = filtered.runFoldMap {
      job => Map(job.acl.department -> (job.res.utime + job.res.stime))
    }.run

    ChartingApp.lowToOthers(cxs, lowPercent)
  }

  def chart = PieChart(data, title = name.localized, legend = false)
}

object CPUTimePerDepartmentPerMonth extends ChartingApp {
  def name = "cputime-per-department"

  override def filtered = interval map { interval ⇒
    dispatched filter startedBetween(interval)
  } getOrElse {
    dispatched
  }

  def data = {
    import ChartingApp.DoubleMonoid

    val pattern = org.joda.time.format.DateTimeFormat.forPattern("MMMM YYYY")

    val cxs = filtered.runFoldMap { job =>
      val month = pattern.print(job.time.start.toLocalDate.withDayOfMonth(1))
      val department = job.acl.department
      val cputime = job.res.utime + job.res.stime
      Map(month -> Map(department -> cputime))
    }.run

    val withOthers = cxs.mapValues(xs => ChartingApp.lowToOthers(xs, lowPercent))

    ChartingApp.inverseCategories(withOthers).toCategoryDataset
  }

  // TODO sort each pie chart by value, put others at front
  def chart = {
    val c = MultiplePieChart(data, title = name.localized)
    c.labelGenerator = None
    c
  }
}

object CPUTimePerDepartmentPerQuarter extends ChartingApp {
  def name = "cputime-per-department"

  override def filtered = interval map { interval ⇒
    dispatched filter startedBetween(interval)
  } getOrElse {
    dispatched
  }

  def data = {
    import ChartingApp.DoubleMonoid

    val cxs = filtered.runFoldMap { job =>
      val quarter = quarter_of_start(job)
      val department = job.acl.department
      val cputime = job.res.utime + job.res.stime
      Map(quarter -> Map(department -> cputime))
    }.run

    val withOthers = cxs.mapValues(xs => ChartingApp.lowToOthers(xs, lowPercent))

    ChartingApp.inverseCategories(withOthers).toCategoryDataset
  }

  // TODO sort each pie chart by value, put others at front
  def chart = {
    val c = MultiplePieChart(data, title = name.localized)
    c.labelGenerator = None
    c
  }
}

object DiskUsage extends ChartingApp {
  def name = "disk-usage-data"

  def data = {
    val GBRE = """([\d.]+)G""".r
    val KBRE = """([\d.]+)K""".r
    val MBRE = """([\d.]+)M""".r
    val TBRE = """([\d.]+)T""".r

    val xys = scalaz.stream.io.linesR("/data/disk-usage.txt").map({ line =>
      val parts = line split "\t"
      val name = parts(1)
      val value = parts(0) match {
        case "0"      ⇒ 0.0
        case KBRE(kb) ⇒ kb.toDouble * math.pow(2,10)
        case MBRE(mb) ⇒ mb.toDouble * math.pow(2,20)
        case GBRE(gb) ⇒ gb.toDouble * math.pow(2,30)
        case TBRE(tb) ⇒ tb.toDouble * math.pow(2,40)
      }
      name -> value
    }).runLog.run
    ChartingApp.lowToOthers(xys.toMap, lowPercent)
  }

  def chart = PieChart(data, title = name.localized, legend = false)
}

object JobsPerUser extends ChartingApp {
  def name = "jobs-per-user"

  def data = filtered.runFoldMap {
    job => Map(job.user.uid -> 1)
  }.run.sortBy(_._2)

  def chart = {
    val chart = BarChart(data)
    chart.title = name.localized
    chart.labelGenerator = CategoryLabelGenerator.Default
    chart
  }
}

object SlotsPerGroup extends ChartingApp {
  def name = "slots-per-group"

  def data = filtered.runFoldMap {
    job => Map(job.acl.department -> job.perMinute(_.slots))
  }.run

  def chart = XYAreaChart.stacked (
    data.toTimeTable,
    title = name.localized
  )
}

object SlotsPerProject extends ChartingApp {
  def name = "slots-per-project"

  def data = filtered.runFoldMap {
    job => Map(job.acl.project -> job.perMinute(_.slots))
  }.run

  def chart = XYAreaChart.stacked (
    data.toTimeTable,
    title = name.localized
  )
}

object SlotsPerQueue extends ChartingApp {
  def name = "slots-per-queue"

  def data = filtered.runFoldMap {
    job => Map(job.queue.get -> job.perMinute(_.slots))
  }.run

  def chart = XYAreaChart.stacked (
    data.toTimeTable,
    title = name.localized
  )
}

object SlotsRunVsWait extends ChartingApp {
  def name = "slots-run-vs-wait"

  override def filtered = {
    val jobs = raw filter realJob filter isDispatched

    interval map { interval =>
      jobs filter isBetween(interval)
    } getOrElse {
      jobs
    }
  }

  def data: Map[String,Map[DateTime,Int]] = {
    val waiting = "waiting".localized
    val running = "running".localized

    filtered.runFoldMap {
      job => Map (
        waiting -> job.perMinute(_.slots),
        running -> job.waitPerMinute(_.slots)
      )
    }.run
  }

  def chart = XYAreaChart.stacked (
    data.toTimeTable,
    title = name.localized
  )
}

object SlotsSequentialVsParallel extends ChartingApp {
  def name = "slots-seq-vs-par"

  def data = filtered.runFoldMap {
    job => Map(SeqVsPar(job) -> job.perMinute(_.slots))
  }.run

  def chart = XYAreaChart.stacked (
    data.toTimeTable,
    title = name.localized
  )
}

object ParallelUsage extends ChartingApp {
  def name = "parallel-usage"

  def data = {
    val a: Map[DateTime,(Int,Int)] = filtered.runFoldMap { job =>
      job perMinute {
        case par if par.parallelEnvironment.isDefined => (par.slots, 0)
        case seq                                      => (0        , 1)
      }
    }.run

    val b: Map[DateTime,Double] = a mapValues {
      case (par,seq) => par.toDouble / ( par + seq )
    }

    // TODO buggy internal TimeSeries time class handling
    // b.toTimeSeriesCollection(name.localized)
    b.toMinuteTimeSeries(name.localized)
  }

  def chart = XYLineChart(data, title = name.localized)
}

// originally: jobs per hour
// throughput: in slots per hour / slots per day
/*
object Throughput extends ChartingApp {
  def name = "throughput"
}
*/

object TurnaroundTime extends ChartingApp {
  implicit val numeric: Numeric[Double] = scala.math.Numeric.DoubleAsIfIntegral

  def name = "turnaround-time"

  override def filtered = interval map { interval ⇒
    dispatched filter submittedBetween(interval)
  } getOrElse {
    dispatched
  }

  def data = filtered.runFoldMap { job =>
    val month = job.time.submission.toLocalDate.withDayOfMonth(1).toDate
    val waitpercent = job.time.waiting.millis.toDouble / job.time.turnaround.millis

    Map(month -> List(waitpercent))
  }.run

  def chart = {
    val chart = XYBoxAndWhiskerChart(data.toBoxAndWhiskerXYDataset())
    chart.title = name.localized
    val axis = chart.plot.getRangeAxis.asInstanceOf[org.jfree.chart.axis.NumberAxis]
    axis.setNumberFormatOverride(java.text.NumberFormat.getPercentInstance)
    chart
  }
}

object Utilization extends ChartingApp {
  def name = "utilization"

  def data = filtered.runFoldMap { job =>
    job perMinute { _.slots }
  }.run

  def chart = XYAreaChart (
    // TODO buggy internal TimeSeries time class handling
    // b.toTimeSeriesCollection(name.localized)
    data.toMinuteTimeSeries(name.localized),
    title = name.localized
  )
}
