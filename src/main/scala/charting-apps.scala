package grid

import language.postfixOps

import org.jfree.chart.JFreeChart

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

object JobsPerUser extends ChartingApp {
  def name = "jobs-per-user"

  val data = interval map { implicit interval ⇒
    dispatched filter { isBetween(_) }
  } getOrElse {
    dispatched
  } groupBy {
    _.user.uid
  } mapValues {
    _.size
  }

  def chart = BarChart (
    title   = name.localized,
    dataset = data.seq.toCategoryDataset,
    labels  = true
  )
}

object SlotsPerGroup extends ChartingApp {
  def name = "slots-per-group"

  def chart = StackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.acl.department } toTimeslots { _.slots } toTimeTable
  )
}

object SlotsPerProject extends ChartingApp {
  def name = "slots-per-project"

  def chart = StackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.acl.project } toTimeslots { _.slots } toTimeTable
  )
}

object SlotsPerQueue extends ChartingApp {
  def name = "slots-per-queue"

  def chart = StackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots } toTimeTable
  )
}

object SlotsRunVsWait extends ChartingApp {
  def name = "slots-run-vs-wait"

  def chart = StackedAreaChart (
    title   = name.localized,
    dataset = (raw filter realJob filter isDispatched toPendingVsRunning) toTimeTable
  )
}

object SlotsSequentialVsParallel extends ChartingApp {
  def name = "slots-seq-vs-par"

  def chart = StackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy SeqVsPar toTimeslots { _.slots } toTimeTable
  )
}

object ParallelUsage extends ChartingApp {
  def name = "parallel-usage"

  import scalaz.Scalaz._

  def chart = LineChart (
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
