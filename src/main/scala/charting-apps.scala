package grid

import language.postfixOps

import org.jfree.chart.JFreeChart
import org.sfree.chart.Charting._

object ChartingApp {
  /** Returns the regex used to parse width and height. */
  def geometry = """(\d+)x(\d+)""".r
}

trait ChartingApp extends AccountingApp {
  /** Returns the chart that will be saved. */
  def chart: JFreeChart

  def defaultExtension = "png"

  def dim = sys.props get "grid.accounting.chart.geometry" collect {
    case ChartingApp.geometry(w,h) ⇒ w.toInt → h.toInt
  } getOrElse 1920 → 1080

  chart.save(extension, output, dim)
}

object CPUTimePerDepartment extends ChartingApp {
  def name = "cputime-per-department"

  def data = interval map { interval ⇒
    dispatched filter isBetween(interval)
  } getOrElse {
    dispatched
  } groupBy department mapValues {
    _.aggregate(0.0)(_ + _.res.cputime, _ + _)
  } sortBy {
    _._2
  }

  def chart = PieChart (
    title   = name.localized,
    dataset = data.toPieDataset,
    legend  = false
  )
}

object CPUTimePerDepartmentPerQuarter extends ChartingApp {
  def name = "cputime-per-department"

  def data = interval map { interval ⇒
    // TODO need seq here because otherwise bug in ParVector
    dispatched.seq filter startedBetween(interval)
  } getOrElse {
    dispatched
  } groupBy quarter_of_start mapValues {
    _ groupBy { department } mapValues { _.aggregate(0.0)(_ + _.res.cputime, _ + _) } sortBy { _._2 }
  } sortBy {
    _._1
  }

  def chart = MultiplePieChart (
    title   = name.localized,
    dataset = data.toCategoryDataset,
    legend  = false
  )
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
    val chart = BarChart (
      title   = name.localized,
      dataset = data.toCategoryDataset
    )
    chart.labels  = true
    chart
  }
}

object SlotsPerGroup extends ChartingApp {
  def name = "slots-per-group"

  def chart = XYAreaChart.stacked (
    title   = name.localized,
    dataset = dispatched groupBy department toTimeslots { _.slots } toTimeTable
  )
}

object SlotsPerProject extends ChartingApp {
  def name = "slots-per-project"

  def chart = XYAreaChart.stacked (
    title   = name.localized,
    dataset = dispatched groupBy project toTimeslots { _.slots } toTimeTable
  )
}

object SlotsPerQueue extends ChartingApp {
  def name = "slots-per-queue"

  def chart = XYAreaChart.stacked (
    title   = name.localized,
    dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots } toTimeTable
  )
}

object SlotsRunVsWait extends ChartingApp {
  def name = "slots-run-vs-wait"

  def chart = XYAreaChart.stacked (
    title   = name.localized,
    dataset = (raw filter realJob filter isDispatched toPendingVsRunning) toTimeTable
  )
}

object SlotsSequentialVsParallel extends ChartingApp {
  def name = "slots-seq-vs-par"

  def chart = XYAreaChart.stacked (
    title   = name.localized,
    dataset = dispatched groupBy SeqVsPar toTimeslots { _.slots } toTimeTable
  )
}

object ParallelUsage extends ChartingApp {
  def name = "parallel-usage"

  import scalaz.Scalaz._

  def chart = XYLineChart (
    title   = name.localized,
    dataset = dispatched.perMinute({
      case par if par.parallelEnvironment.isDefined ⇒ (par.slots, 0)
      case seq                                      ⇒ (0        , 1)
    }).fold(Map()) {
      _ ⊹ _
    } mapValues {
      case (p,s) ⇒ p.toDouble / (p+s)
    } toTimeSeriesCollection name.localized
  )
}
