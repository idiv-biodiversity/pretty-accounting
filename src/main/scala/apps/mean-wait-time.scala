package grid

import cats.implicits._
import fs2._

// TODO localize
object `mean-wait-time` extends AccAppNG("pa-mean-wait-time") with Streamy {

  // --------------------------------------------------------------------------
  // data
  // --------------------------------------------------------------------------

  type Data = Map[String, Vector[Long]]

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    // TODO cache (although once per file is ok)
    val categoryOf: Job => String = categoryF

    stream map { job =>
      val category = categoryOf(job)
      val wait = job.time.waiting.millis // .toPeriod

      Map(category -> Vector(wait))
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

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
  }

}
