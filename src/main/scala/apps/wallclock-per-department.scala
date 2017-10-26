package grid

import cats.implicits._
import fs2._

// TODO localize
object `wallclock-per-department` extends AccAppNG("pa-wallclock-pe-department") with Streamy {

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
      val group = job.acl.department
      val millis = job.time.running.millis * job.slots

      Map(group -> millis)
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
          chart.saveAsPNG(s"/tmp/heavy-hitters-idiv-group-$p.png", conf.resolution) // TODO conf
        }

      case Output.CSV =>
        Console.err.println(s"output ${conf.output} not supported")

      case Output.Table =>
        data.sortBy(_._2) foreach {
          case (group, millis) =>
            println(s"""$group $millis""")
        }
    }
  }

}
