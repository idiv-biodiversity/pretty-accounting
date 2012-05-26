package grid

import org.jfree.chart.ChartUtilities._

trait ChartingApp extends AccountingApp {
  def defaultExtension = "png"

  /** Returns the regex used to parse width and height. */
  protected lazy val geometry = """(\d+)x(\d+)""".r

  implicit lazy val dim = sys.props get "grid.accounting.chart.geometry" collect {
    case geometry(w,h) => w.toInt -> h.toInt
  } getOrElse 1920 -> 1080
}

object JobsPerUser extends ChartingApp {
  def name = "jobs-per-user"

  implicit val title = "Jobs per User"
  implicit val dataset: CategoryDataset = interval map { implicit interval =>
    dispatched filter { isBetween(_) }
  } getOrElse {
    dispatched
  } groupBy {
    _.user.uid
  } mapValues {
    _.size
  }

  createLabelledBarChart saveAs extension
}

object SlotsPerQueue extends ChartingApp {
  override lazy val name = "slots-per-queue"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots }
  ) saveAs extension
}

object SlotsRunVsWait extends ChartingApp {
  override lazy val name = "slots-run-vs-wait"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = raw filter realJob filter isDispatched toPendingVsRunning
  ) saveAs extension
}

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "slots-seq-vs-par"

  createTimeSeriesStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy {
      j => if (parallel(j)) "parallel".localized else "sequential".localized
    } toTimeslots {
      _.slots
    }
  ) saveAs extension
}
