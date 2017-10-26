package grid

import cats.implicits._
import fs2._
import scalax.cli.Table
import scalax.cli.Table.Alignment

object `efficiency` extends AccAppNG("pa-efficiency") with Streamy {

  // --------------------------------------------------------------------------
  // data
  // --------------------------------------------------------------------------

  type Data = Map[String, Vector[Double]]

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    // TODO cache (although once per file is ok)
    val categoryOf: Job => String = categoryF

    stream map { job =>
      val category = categoryOf(job)
      val data = Vector(job.efficiency)
      Map(category -> data)
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    conf.output match {
      case Output.Chart =>
        ???

      case Output.CSV =>
        ???

      case Output.Table =>
        val table = Table("", "# Jobs", "∅ Efficiency in %") // TODO localize
        table.alignments(1) = Alignment.Right
        table.alignments(2) = Alignment.Right

        for ((category, values) <- data.sortBy(_._1)) {
          val mean = values.sum / values.size
          val row = Seq(category, s"${values.size}", s"${(mean * 100).round}")
          table.rows += row
        }

        table.print()
    }
  }

}
