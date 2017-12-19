package grid

import cats.Monoid
import cats.implicits._
import fs2._

object `users` extends AccAppNG("pa-users") with Streamy {

  type Data = Map[LocalDate, Set[String]]

  import TimeConverter.jodaToJFreeMonth

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    stream map { job =>
      val uid = job.user.uid
      val date = job.time.start.toLocalDate.withDayOfMonth(1)

      job.acl.project.filter(_ === "idiv").fold(Map[LocalDate, Set[String]]()) { _ =>
        Map(date -> Set(uid))
      }
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    val dataset = data.mapValues(_.size).toTimeSeries(
      "active users"
    )

    val chart = XYLineChart(dataset)

    chart.saveAsPNG("/tmp/users.png", conf.resolution)
  }

}
