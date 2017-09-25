package grid

import java.nio.file.Paths

import cats.implicits._
import fs2._
import fs2.interop.cats._
import scalax.chart.Chart

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

@deprecated("use new config", "")
trait ChartingApp extends AccountingApp {
  /** Returns the chart that will be saved. */
  def chart: Chart

  override def defaultExtension = "png"

  def lowPercent = sys.props.getOrElse("grid.accounting.chart.pie.exclude", "0.01").toDouble

  extension.toLowerCase match {
    // case "pdf" ⇒ chart.saveAsPDF(output, conf.resolution)
    case "png" ⇒ chart.saveAsPNG(output, conf.resolution)
    case "jpg" | "jpeg" ⇒ chart.saveAsJPEG(output, conf.resolution)
  }
}

object `cputime-per-department` extends ChartingApp {
  override def name = "cputime-per-department"

  def data = {
    val cxs = filtered.runFoldMap {
      job => Map(job.acl.department -> (job.res.utime + job.res.stime))
    }.unsafeRun

    ChartingApp.lowToOthers(cxs, lowPercent)
  }

  def chart = {
    val c = PieChart(data)
    c.title = name.localized
    c.subtitles.clear()
    c
  }
}

object `cputime-per-department-per-month` extends ChartingApp {
  override def name = "cputime-per-department"

  override def filtered = conf.interval map { interval ⇒
    dispatched filter (started between interval)
  } getOrElse {
    dispatched
  }

  def data = {
    val pattern = org.joda.time.format.DateTimeFormat.forPattern("MMMM YYYY")

    val cxs = filtered.runFoldMap { job =>
      val month = pattern.print(job.time.start.toLocalDate.withDayOfMonth(1))
      val department = job.acl.department
      val cputime = job.res.utime + job.res.stime
      Map(month -> Map(department -> cputime))
    }.unsafeRun

    val withOthers = cxs.mapValues(xs => ChartingApp.lowToOthers(xs, lowPercent))

    ChartingApp.inverseCategories(withOthers).toCategoryDataset
  }

  // TODO sort each pie chart by value, put others at front
  def chart = {
    val c = MultiplePieChart(data)
    c.title = name.localized
    c.labelGenerator = None
    c
  }
}

object `cputime-per-department-per-quarter` extends ChartingApp {
  override def name = "cputime-per-department"

  override def filtered = conf.interval map { interval ⇒
    dispatched filter (started between interval)
  } getOrElse {
    dispatched
  }

  def data = {
    val cxs = filtered.runFoldMap { job =>
      val quarter = quarter_of_start(job)
      val department = job.acl.department
      val cputime = job.res.utime + job.res.stime
      Map(quarter -> Map(department -> cputime))
    }.unsafeRun

    val withOthers = cxs.mapValues(xs => ChartingApp.lowToOthers(xs, lowPercent))

    ChartingApp.inverseCategories(withOthers).toCategoryDataset
  }

  // TODO sort each pie chart by value, put others at front
  def chart = {
    val c = MultiplePieChart(data)
    c.title = name.localized
    c.labelGenerator = None
    c
  }
}

object `disk-usage` extends ChartingApp {
  override def name = "disk-usage-data"

  def data = {
    val GBRE = """([\d.]+)G""".r
    val KBRE = """([\d.]+)K""".r
    val MBRE = """([\d.]+)M""".r
    val TBRE = """([\d.]+)T""".r

    // TODO remove chunk size magic number / make customizable in app
    val bytes = io.file.readAll[Task](Paths.get("/data/disk-usage.txt"), chunkSize = math.pow(2,20).toInt)

    val lines = bytes.through(text.utf8Decode).through(text.lines)

    val xys = lines.map({ line =>
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
    }).runLog.unsafeRun

    ChartingApp.lowToOthers(xys.toMap, lowPercent)
  }

  def chart = {
    val c = PieChart(data)
    c.title = name.localized
    c.subtitles.clear()
    c
  }
}

object `jobs-per-user` extends ChartingApp {
  override def name = "jobs-per-user"

  def data = filtered.runFoldMap {
    job => Map(job.user.uid -> 1)
  }.unsafeRun.sortBy(_._2)

  def chart = {
    val c = BarChart(data)
    c.title = name.localized
    c.labelGenerator = CategoryLabelGenerator.Default
    c
  }
}

object `slots-per-group` extends ChartingApp {
  override def name = "slots-per-group"

  def data = filtered.runFoldMap {
    job => Map(job.acl.department -> job.perMinute(_.slots))
  }.unsafeRun

  def chart = {
    import TimeConverter.jodaToJFreeMinute
    val c = XYAreaChart.stacked(data.toTimeTable)
    c.title = name.localized
    c
  }
}

object `slots-per-project` extends ChartingApp {
  override def name = "slots-per-project"

  def data = filtered.runFoldMap {
    job => Map(job.acl.project.getOrElse("NONE") -> job.perMinute(_.slots)) // TODO localize NONE
  }.unsafeRun

  def chart = {
    import TimeConverter.jodaToJFreeMinute
    val c = XYAreaChart.stacked(data.toTimeTable)
    c.title = name.localized
    c
  }
}

object `slots-per-queue` extends ChartingApp {
  override def name = "slots-per-queue"

  def data = filtered.runFoldMap {
    job => Map(job.queue.get -> job.perMinute(_.slots))
  }.unsafeRun

  def chart = {
    import TimeConverter.jodaToJFreeMinute
    val c = XYAreaChart.stacked(data.toTimeTable)
    c.title = name.localized
    c
  }
}

object `slots-run-vs-wait` extends ChartingApp {
  override def name = "slots-run-vs-wait"

  override def filtered = {
    val jobs = raw filter realJob filter isDispatched

    conf.interval map { interval =>
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
    }.unsafeRun
  }

  def chart = {
    import TimeConverter.jodaToJFreeMinute
    val c = XYAreaChart.stacked(data.toTimeTable)
    c.title = name.localized
    c
  }
}

object `slots-sequential-vs-parallel` extends ChartingApp {
  override def name = "slots-seq-vs-par"

  def data = filtered.runFoldMap {
    job => Map(SeqVsPar(job) -> job.perMinute(_.slots))
  }.unsafeRun

  def chart = {
    import TimeConverter.jodaToJFreeMinute
    val c = XYAreaChart.stacked(data.toTimeTable)
    c.title = name.localized
    c
  }
}

object `parallel-usage` extends ChartingApp {
  override def name = "parallel-usage"

  def data = {
    import TimeConverter.jodaToJFreeMinute

    val a: Map[DateTime,(Int,Int)] = filtered.runFoldMap { job =>
      job perMinute {
        case par if par.parallelEnvironment.isDefined => (par.slots, 0)
        case _                                        => (0        , 1)
      }
    }.unsafeRun

    val b: Map[DateTime,Double] = a mapValues {
      case (par,seq) => par.toDouble / ( par + seq )
    }

    // TODO buggy internal TimeSeries time class handling
    // b.toTimeSeriesCollection(name.localized)
    b.toMinuteTimeSeries(name.localized)
  }

  def chart = {
    val c = XYLineChart(data)
    c.title = name.localized
    c
  }
}

// originally: jobs per hour
// throughput: in slots per hour / slots per day
/*
object Throughput extends ChartingApp {
  override def name = "throughput"
}
*/

object `turnaroundtime` extends ChartingApp {
  implicit val numeric: Numeric[Double] = scala.math.Numeric.DoubleAsIfIntegral

  override def name = "turnaround-time"

  override def filtered = conf.interval map { interval ⇒
    dispatched filter (submitted between interval)
  } getOrElse {
    dispatched
  }

  def data = filtered.runFoldMap { job =>
    val month = job.time.submission.toLocalDate.withDayOfMonth(1).toDate
    val waitpercent = job.time.waiting.millis.toDouble / job.time.turnaround.millis

    Map(month -> List(waitpercent))
  }.unsafeRun

  def chart = {
    val chart = XYBoxAndWhiskerChart(data.toBoxAndWhiskerXYDataset())
    chart.title = name.localized
    val axis = chart.plot.getRangeAxis.asInstanceOf[org.jfree.chart.axis.NumberAxis]
    axis.setNumberFormatOverride(java.text.NumberFormat.getPercentInstance)
    chart
  }
}

object `utilization` extends ChartingApp {
  override def name = "utilization"

  def data = filtered.runFoldMap { job =>
    job perMinute { _.slots }
  }.unsafeRun

  def chart = {
    import TimeConverter.jodaToJFreeMinute
    // TODO buggy internal TimeSeries time class handling
    // b.toTimeSeriesCollection(name.localized)
    val c = XYAreaChart(data.toMinuteTimeSeries(name.localized))
    c.title = name.localized
    c
  }
}
