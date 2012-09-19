package grid

import language.postfixOps

object ChartingApp {
  /** Returns the regex used to parse width and height. */
  def geometry = """(\d+)x(\d+)""".r
}

trait ChartingApp extends AccountingApp {
  def defaultExtension = "png"

  implicit lazy val dim = sys.props get "grid.accounting.chart.geometry" collect {
    case ChartingApp.geometry(w,h) ⇒ w.toInt → h.toInt
  } getOrElse 1920 → 1080
}

object JobsPerUser extends ChartingApp {
  override lazy val name = "jobs-per-user"

  val data = interval map { implicit interval ⇒
    dispatched filter { isBetween(_) }
  } getOrElse {
    dispatched
  } groupBy {
    _.user.uid
  } mapValues {
    _.size
  }

  createBarChart (
    title   = name.localized,
    dataset = data.seq.toCategoryDataset,
    labels  = true
  ) save ( extension, output, dim )
}

object SlotsPerQueue extends ChartingApp {
  override lazy val name = "slots-per-queue"

  createStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots } toTimeTable
  ) save ( extension, output, dim )
}

object SlotsRunVsWait extends ChartingApp {
  override lazy val name = "slots-run-vs-wait"

  createStackedAreaChart (
    title   = name.localized,
    dataset = (raw filter realJob filter isDispatched toPendingVsRunning) toTimeTable
  ) save ( extension, output, dim )
}

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "slots-seq-vs-par"

  createStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy SeqVsPar toTimeslots { _.slots } toTimeTable
  ) save ( extension, output, dim )
}

object ParallelUsage extends ChartingApp {
  override lazy val name = "parallel-usage"

  import scalaz.Scalaz._

  val xs = dispatched perMinute {
    case par if par.parallelEnvironment.isDefined ⇒
      (par.slots, 0)
    case seq ⇒
      (0,1)
  }

  val data = xs.fold(Map())(_ ⊹ _) mapValues {
    case (p,s) ⇒ p.toDouble / (p+s)
  }

  createLineChart (
    title   = name.localized,
    dataset = data.toTimeSeriesCollection("name.localized")
  ) save ( extension, output, dim )
}
