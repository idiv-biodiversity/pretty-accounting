package grid

import cats._
import cats.implicits._
import fs2._
import fs2.interop.cats._

// TODO localize
object `wallclock-per-user` extends AccAppNG("pa-wallclock-per-user") with Streamy {

  self =>

  // --------------------------------------------------------------------------
  // data
  // --------------------------------------------------------------------------

  type Data = Map[String, Long]

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    // TODO move to general filter location
    stream filter { job =>
      conf.filter.project.fold(true) { p =>
        job.acl.project.fold(false)(_ === p)
      }
    } map { job =>
      val user = job.user.uid

      job.time.running match {
        case Right(running) =>
          Map(user -> (running.millis * job.slots))

        case Left(message) =>
          Console.err.println(
            s"""${self.name}: ignoring job: $message"""
          )
          Map()
      }
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    conf.output match {
      case Output.Chart =>
        for (p <- Seq(0.01, 0.02, 0.05, 0.1)) {
          // TODO make pretty stuff general
          val pretty = {
            val sum = data.foldLeft(0L)(_ + _._2)
            val (merge, keep) = data.partition(_._2 < sum * p)
            val others = s"""${"others".localized} (${merge.size})""" -> merge.foldLeft(0L)(_ + _._2)
            others +: keep.sortBy(_._2)
          }

          val chart = PieChart(pretty.toPieDataset)
          chart.title = "iDiv Heavy Hitters" // TODO localize
          chart.subtitles.clear()
          chart.saveAsPNG(s"/tmp/heavy-hitters-idiv-2-$p.png", conf.resolution) // TODO conf
        }

      case Output.CSV =>
        Console.err.println(s"output ${conf.output} not supported")

      case Output.Table =>
        data.sortBy(_._2) foreach {
          case (user, millis) =>
            println(s"""$user $millis""")
        }
    }
  }

}
