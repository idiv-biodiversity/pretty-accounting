package grid

object ChartingApp {
  /** Returns the regex used to parse width and height. */
  def geometry = """(\d+)x(\d+)""".r
}

trait ChartingApp extends AccountingApp {
  def defaultExtension = "png"

  implicit lazy val dim = sys.props get "grid.accounting.chart.geometry" collect {
    case ChartingApp.geometry(w,h) => w.toInt -> h.toInt
  } getOrElse 1920 -> 1080
}

object JobsPerUser extends ChartingApp {
  override lazy val name = "jobs-per-user"

  createLabelledBarChart (
    title   = name.localized,
    dataset = interval map { implicit interval =>
      dispatched filter { isBetween(_) }
    } getOrElse {
      dispatched
    } groupBy {
      _.user.uid
    } mapValues {
      _.size
    }
  ) saveAs extension
}

object SlotsPerQueue extends ChartingApp {
  override lazy val name = "slots-per-queue"

  createStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy { _.queue.get } toTimeslots { _.slots }
  ) saveAs extension
}

object SlotsRunVsWait extends ChartingApp {
  override lazy val name = "slots-run-vs-wait"

  createStackedAreaChart (
    title   = name.localized,
    dataset = raw filter realJob filter isDispatched toPendingVsRunning
  ) saveAs extension
}

object SlotsSequentialVsParallel extends ChartingApp {
  override lazy val name = "slots-seq-vs-par"

  createStackedAreaChart (
    title   = name.localized,
    dataset = dispatched groupBy {
      j => if (parallel(j)) "parallel".localized else "sequential".localized
    } toTimeslots {
      _.slots
    }
  ) saveAs extension
}

object ParallelUsage extends ChartingApp {
  override lazy val name = "parallel-usage"

  val x: GenIterable[Map[DateTime,(Int,Int)]] = dispatched perMinute {
    case par if par.parallelEnvironment.isDefined =>
      (par.slots, 0)
    case seq =>
      (0,1)
  }

  import scalaz.Scalaz._

  createLineChart (
    title   = name.localized,
    dataset = x.fold(Map())(_ |+| _) mapValues {
      case (p,s) => p.toDouble / (p+s)
    }
  ) saveAs extension
}
