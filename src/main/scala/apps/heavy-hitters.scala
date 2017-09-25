package grid

import cats._
import cats.implicits._
import fs2._
import fs2.interop.cats._

// top n (3) users each month
// TODO localize
object `heavy-hitters` extends App with Accounting {

  case class Data(cputime: Double, wctime: Double) {
    def +(that: Data) = {
      Data(cputime = this.cputime + that.cputime, wctime = this.wctime + that.wctime)
    }
  }

  object Data {
    val empty = Data(0, 0)
    implicit val DataMonoid: Monoid[Data] = new Monoid[Data] {
      val empty = Data.empty
      def combine(x: Data, y: Data) = x + y
    }
  }

  type GroupedData = ((LocalDate, String), Data)

  Config.parser(name = "pa-heavy-hitters").parse(args, Config.empty) match {
    case Some(conf) =>
      val (threads, strategy) = conf.strategy
      implicit val S = strategy

      val streams: Stream[Task, Stream[Task, GroupedData]] =
        Stream(conf.accountingFiles: _*) map { file =>
          val proto = raw(file).filter(job => realJob(job) && job.res.cputime > 0 && conf.startedBetween(job))

          conf.mapWithProgress(proto, file) { job =>
            val date = job.time.start.toLocalDate.withDayOfMonth(1)
            val user = job.user.uid
            val data = Data(job.res.cputime, job.res.wctime * job.slots)

            (date, user) -> data
          }
        }

      val stream: Stream[Task, GroupedData] =
        fs2.concurrent.join(maxOpen = threads)(streams)

      val data: Map[LocalDate, Map[String, Data]] = stream
        .runFoldMap({ a =>
          val ((date, user), data) = a
          Map(date -> Map(user -> data))
        })
        .unsafeRun

      data.sortBy(_._1) foreach {
        case (date, userdata) =>
          println(date)
          for {
            (user, data) <- userdata.sortBy(-_._2.wctime).take(3) // TODO take n (conf)
            days = (data.wctime / 60 / 60 / 24).round
            if days > 0
          } println(s"$user $days days")
      }

    case None =>
      sys.exit(1)
  }

}
