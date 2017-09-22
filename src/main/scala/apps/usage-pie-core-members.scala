package grid

import cats._
import cats.implicits._
import fs2._
import fs2.interop.cats._

// TODO localize
object `usage-core-members` extends App with Accounting {

  Config.parser(name = "pa-usage-core-members").parse(args, Config.empty) match {
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
            val department = job.acl.department
            val association = conf.association("idiv")(department) // TODO conf
            val millis = (job.time.start to job.time.end).millis * job.slots

            Map(association -> millis)
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
          val pretty = data.map({ kv =>
            val (assoc, value) = kv
            assoc.toString -> value
          }).sortBy(_._2)

          val chart = PieChart(pretty.toPieDataset)
          chart.subtitles.clear()
          chart.saveAsPNG("/tmp/idiv-group-association.png", conf.resolution) // TODO conf

        case Output.CSV =>
          Console.err.println(s"output ${conf.output} not supported")

        case Output.Table =>
          data.sortBy(_._2) foreach {
            case (association, millis) =>
              println(s"""$association $millis""")
          }
      }

    case None =>
      sys.exit(1)
  }

}
