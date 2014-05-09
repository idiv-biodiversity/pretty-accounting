package grid

import language.postfixOps

import scalax.chart._

object ChartingApp {
  /** Returns the regex used to parse width and height. */
  def geometry = """(\d+)x(\d+)""".r
}

trait ChartingApp extends AccountingApp {
  /** Returns the chart that will be saved. */
  def chart: Chart

  def defaultExtension = "png"

  def excludePercent = sys.props.getOrElse("grid.accounting.chart.pie.exclude", "0.01").toDouble

  def dim = sys.props get "grid.accounting.chart.geometry" collect {
    case ChartingApp.geometry(w,h) ⇒ w.toInt → h.toInt
  } getOrElse 1920 → 1080

  extension.toLowerCase match {
    case "pdf" ⇒ chart.saveAsPDF(output, dim)
    case "png" ⇒ chart.saveAsPNG(output, dim)
    case "jpg" | "jpeg" ⇒ chart.saveAsJPEG(output, dim)
  }
}

object CPUTimePerDepartment extends ChartingApp {
  def name = "cputime-per-department"

  def data = {
    val cxs = interval map { interval ⇒
      dispatched filter isBetween(interval)
    } getOrElse {
      dispatched
    } groupBy department mapValues {
      _.aggregate(0.0)(_ + _.res.cputime, _ + _)
    }
    val sum = cxs.aggregate(0.0)(_ + _._2, _ + _)
    cxs filter { _._2 / sum > excludePercent } sortBy { _._2 }
  }

  def chart = PieChart(data, title = name.localized, legend = false)
}

object CPUTimePerDepartmentPerMonth extends ChartingApp {
  def name = "cputime-per-department"

  def data = interval map { interval ⇒
    dispatched.seq filter startedBetween(interval)
  } getOrElse {
    dispatched
  } groupBy month_of_start mapValues { quarterly ⇒
    val cxs = quarterly groupBy { department } mapValues { _.aggregate(0.0)(_ + _.res.cputime, _ + _) }
    val sum = cxs.aggregate(0.0)(_ + _._2, _ + _)
    cxs filter { _._2 / sum > excludePercent } sortBy { _._2 }
  } sortBy {
    _._1
  } map { case(k,v) ⇒
    org.joda.time.format.DateTimeFormat.forPattern("MMMM YYYY").print(k) → v
  }

  // TODO sort each pie chart by value
  def chart = MultiplePieChart(data, title = name.localized, legend = false)
}

object CPUTimePerDepartmentPerQuarter extends ChartingApp {
  def name = "cputime-per-department"

  def data = interval map { interval ⇒
    dispatched filter startedBetween(interval)
  } getOrElse {
    dispatched
  } groupBy quarter_of_start mapValues { quarterly ⇒
    val cxs = quarterly groupBy { department } mapValues { _.aggregate(0.0)(_ + _.res.cputime, _ + _) }
    val sum = cxs.aggregate(0.0)(_ + _._2, _ + _)
    cxs filter { _._2 / sum > excludePercent } sortBy { _._2 }
  } sortBy {
    _._1
  }

  // TODO sort each pie chart by value
  def chart = MultiplePieChart(data, title = name.localized, legend = false)
}

object DiskUsage extends ChartingApp {
  def name = "disk-usage-data"

  def data = {
    val GBRE = """([\d.]+)G""".r
    val MBRE = """([\d.]+)M""".r
    val TBRE = """([\d.]+)T""".r

    val xys = scalax.io.Resource.fromFile("/tmp/disk-usage.txt").lines().par map { line ⇒
      val ps = line.split("\t")
      ps(1) → (ps(0) match {
        case MBRE(mb) ⇒ mb.toDouble * math.pow(2,20)
        case GBRE(gb) ⇒ gb.toDouble * math.pow(2,30)
        case TBRE(tb) ⇒ tb.toDouble * math.pow(2,40)
      }).round
    }
    val sum = xys.aggregate(0.0)(_ + _._2, _ + _)
    xys filter { _._2 / sum > excludePercent } sortBy { _._2 }
  }

  def chart = PieChart(data, title = name.localized, legend = false)
}

object JobsPerUser extends ChartingApp {
  def name = "jobs-per-user"

  def data = interval map { interval ⇒
    dispatched filter isBetween(interval)
  } getOrElse {
    dispatched
  } groupBy {
    owner
  } mapValues {
    _.size
  } sortBy {
    _._2
  }

  def chart = {
    val chart = BarChart(data)
    chart.title = name.localized
    chart.labelGenerator = CategoryLabelGenerator.Default
    chart
  }
}

object SlotsPerGroup extends ChartingApp {
  def name = "slots-per-group"

  def chart = XYAreaChart.stacked (
    title = name.localized,
    data  = dispatched groupBy department toTimeslots { _.slots } toTimeTable
  )
}

object SlotsPerProject extends ChartingApp {
  def name = "slots-per-project"

  def chart = XYAreaChart.stacked (
    title = name.localized,
    data  = dispatched groupBy project toTimeslots { _.slots } toTimeTable
  )
}

object SlotsPerQueue extends ChartingApp {
  def name = "slots-per-queue"

  def chart = XYAreaChart.stacked (
    title = name.localized,
    data  = dispatched groupBy { _.queue.get } toTimeslots { _.slots } toTimeTable
  )
}

object SlotsRunVsWait extends ChartingApp {
  def name = "slots-run-vs-wait"

  def chart = XYAreaChart.stacked (
    title = name.localized,
    data  = (raw filter realJob filter isDispatched toPendingVsRunning) toTimeTable
  )
}

object SlotsSequentialVsParallel extends ChartingApp {
  def name = "slots-seq-vs-par"

  def chart = XYAreaChart.stacked (
    title = name.localized,
    data  = dispatched groupBy SeqVsPar toTimeslots { _.slots } toTimeTable
  )
}

object ParallelUsage extends ChartingApp {
  def name = "parallel-usage"

  import scalaz.Scalaz._

  def chart = XYLineChart (
    title = name.localized,
    data = dispatched.perMinute({
      case par if par.parallelEnvironment.isDefined ⇒ (par.slots, 0)
      case seq                                      ⇒ (0        , 1)
    }).fold(Map()) {
      _ ⊹ _
    } mapValues {
      case (p,s) ⇒ p.toDouble / (p+s)
    } toTimeSeriesCollection name.localized
  )
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

  def data: collection.GenTraversableOnce[(java.util.Date,collection.Seq[Double])] = interval map { interval ⇒
    dispatched filter submittedBetween(interval)
  } getOrElse {
    dispatched
  } groupBy month_of_submission map { case (date,js) ⇒
    date.toDate → (js.map(j ⇒ j.time.waiting.millis.toDouble / j.time.turnaround.millis).toList)
  }

  def chart = {
    val chart = XYBoxAndWhiskerChart(data.toBoxAndWhiskerXYDataset())
    chart.title = name.localized
    val axis = chart.plot.getRangeAxis.asInstanceOf[org.jfree.chart.axis.NumberAxis]
    axis.setNumberFormatOverride(java.text.NumberFormat.getPercentInstance)
    chart
  }
}

// keep the CPU busy all time
object Utilization extends ChartingApp {
  def name = "utilization"

  def chart = XYAreaChart (
    title = name.localized,
    data  = dispatched.toTimeslots(_.slots).toTimeSeriesCollection("")
  )
}
