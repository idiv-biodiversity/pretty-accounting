package grid

import cats._
import cats.implicits._
import fs2._
import fs2.interop.cats._

// TODO localize
object `wallclock-per-user` extends App with Accounting {

  type GroupedData = (String, Long)

  Config.parser(name = "pa-wallclock-per-user").parse(args, Config.empty) match {
    case Some(conf) =>
      val (threads, strategy) = conf.strategy
      implicit val S = strategy

      type Data = Map[String, Long]

      val streams: Stream[Task, Stream[Task, Data]] =
        Stream(conf.accountingFiles: _*) map { file =>
          val proto = raw(file) filter { job =>
            conf.startedBetween(job) &&
            job.acl.project == Some("idiv") // TODO conf
          }

          conf.mapWithProgress(proto, file) { job =>
            val user = job.user.uid
            val millis = job.time.running.millis * job.slots

            Map(user -> millis)
          }
        }

      val stream: Stream[Task, Data] =
        fs2.concurrent.join(maxOpen = threads)(streams)

      val x: Task[Data] =
        stream.runFoldMap(identity)

      val data: Data =
        x.unsafeRun()

      conf.output match {
        case Output.Chart =>
          for (p <- Seq(0.01, 0.02, 0.05, 0.1)) {
            // TODO make pretty stuff general
            val pretty = {
              val sum = data.foldLeft(0L)(_ + _._2)
              val (merge, keep) = data.partition(_._2 < sum * p)
              // TODO localize other
              val other = s"""other (${merge.size})""" -> merge.foldLeft(0L)(_ + _._2)
              other +: keep.sortBy(_._2)
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

    case None =>
      sys.exit(1)
  }

}
