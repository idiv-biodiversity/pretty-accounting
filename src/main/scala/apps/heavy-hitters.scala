package grid

import cats.Monoid
import cats.implicits._
import fs2._

// top n (3) users each month
// TODO localize
object `heavy-hitters` extends AccAppNG("pa-heavy-hitters") with Streamy {

  // --------------------------------------------------------------------------
  // data
  // --------------------------------------------------------------------------

  case class Base(cputime: Double, wctime: Double) {
    def +(that: Base) = {
      Base(cputime = this.cputime + that.cputime, wctime = this.wctime + that.wctime)
    }
  }

  object Base {
    val empty = Base(0, 0)
    implicit val BaseMonoid: Monoid[Base] = new Monoid[Base] {
      val empty = Base.empty
      def combine(x: Base, y: Base) = x + y
    }
  }

  type Data = Map[LocalDate, Map[String, Base]]

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    stream map { job =>
      val date = job.time.start.toLocalDate.withDayOfMonth(1)
      val user = job.user.uid
      val data = Base(job.res.cputime, job.res.wctime * job.slots)

      Map(date -> Map(user -> data))
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    data.sortBy(_._1) foreach {
      case (date, userdata) =>
        println(date)
        for {
          (user, data) <- userdata.sortBy(-_._2.wctime).take(3) // TODO take n (conf)
          days = (data.wctime / 60 / 60 / 24).round
          if days > 0
        } println(s"$user $days days")
    }
  }

}
