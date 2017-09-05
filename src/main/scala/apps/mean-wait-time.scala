package grid

import cats._
import cats.implicits._
import fs2._
import fs2.interop.cats._
import java.lang.Math.addExact

// TODO localize
object `mean-wait-time` extends App with Accounting {

  type Data = Map[String, Vector[Long]]

  Config.parser(name = "pa-usage-core-members").parse(args, Config.empty) match {
    case Some(conf) =>
      val (threads, strategy) = conf.strategy
      implicit val S = strategy

      val categoryOf: Job => String = conf.category match {
        case Some(Category.Department) =>
          val f = (job: Job) => job.acl.department
          f

        case Some(Category.Project) =>
          // TODO localize NONE
          (job: Job) => job.acl.project.getOrElse("NONE")

        case None =>
          // TODO if uncategorized or only one category left, don't print table
          // TODO localize
          val f = (_: Job) => "uncategorized"
          f
      }

      val streams: Stream[Task, Stream[Task, Data]] =
        Stream(conf.accountingFiles: _*) map { file =>
          val proto = raw(file) filter { job =>
            conf.startedBetween(job) &&
            (job.time.start > job.time.submission)
          }

          conf.mapWithProgress(proto, file) { job =>
            val category = categoryOf(job)
            val wait = job.time.waiting.millis // .toPeriod

            Map(category -> Vector(wait))
          }
        }

      val stream: Stream[Task, Data] =
        fs2.concurrent.join(maxOpen = threads)(streams)

      val x: Task[Data] =
        stream.runFoldMap(identity)

      val data: Data =
        x.unsafeRun() - "NONE"

      conf.output match {
        case Output.Chart =>
          data.mapValues(_.size) foreach println
          // idiv 4 mio ufz 17 mio

          // TODO takes ages
          val chart = BoxAndWhiskerChart(data.toBoxAndWhiskerCategoryDataset[Long](""))
          chart.saveAsPNG(s"/tmp/waiting-time-${conf.category.toString.toLowerCase}.png", conf.resolution) // TODO conf

        case Output.CSV =>
          for ((category, values) <- data) {
            val filename = s"""/tmp/waiting-time-$category.csv"""
            val file = new java.io.PrintStream(filename)
            for (value <- values) {
              file.println(value)
            }
            file.close()
          }

        case Output.Table =>
          for ((category, values) <- data.sortBy(_._1)) {
            val mean = values.sum / values.size

            println(s"""$category $mean""")
          }
      }

    case None =>
      sys.exit(1)
  }

}
