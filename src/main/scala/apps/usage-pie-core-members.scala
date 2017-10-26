package grid

import cats.implicits._
import fs2._

// TODO localize
object `usage-core-members` extends AccAppNG("pa-usage-core-members") with Streamy {

  type Data = Map[String, Long]

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    stream filter { job =>
      job.acl.project == Some("idiv") // TODO conf
    } map { job =>
      val department = job.acl.department
      val association = conf.association("idiv")(department) // TODO conf
      val millis = (job.time.start to job.time.end).millis * job.slots

      Map(association -> millis)
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

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
  }

}
