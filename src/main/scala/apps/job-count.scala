package grid

import cats._
import cats.implicits._
import fs2._
import scalax.cli.Table
import scalax.cli.Table.Alignment

object `job-count` extends AccAppNG("pa-job-count") with Streamy with Tabular {

  type B = Long

  lazy val MB = implicitly[Monoid[B]]
  lazy val MD = implicitly[Monoid[Data]]
  lazy val OB = implicitly[Ordering[B]]

  final
  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    // TODO cache (although once per file is ok)
    val categoryOf: Job => String = categoryF

    stream map { job =>
      val category = categoryOf(job)
      val data = 1L
      Map(category -> data)
    }
  }

  def p(data: Iterable[Grouped]): Unit = {
    val pretty: Iterable[Seq[String]] = for {
      (project, value) <- data
    } yield {
      Seq(s"$project", s"$value")
    }

    val table = Table("Category", "Jobs") // TODO localize
    table.alignments(1) = Alignment.Right
    for (row <- pretty) {
      table.rows += row
    }
    table.print()
  }

}
