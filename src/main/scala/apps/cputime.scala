package grid

import cats.Monoid
import cats.implicits._
import fs2._
import scalax.cli.Table
import scalax.cli.Table.Alignment

// TODO localize
object cputime extends AccAppNG("pa-cputime") with Streamy {

  // --------------------------------------------------------------------------
  // data
  // --------------------------------------------------------------------------

  final case class Base(inclusive: Double = 0.0, exclusive: Double = 0.0)

  object Base {
    val empty = Base()
    implicit val BaseMonoid: Monoid[Base] = new Monoid[Base] {
      override def empty: Base = Base.empty
      override def combine(x: Base, y: Base): Base =
        Base(x.inclusive + y.inclusive, x.exclusive + y.exclusive)
    }
  }

  type Data = Map[String, Base]
  type Grouped = (String, Base)

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  private
  def inclexcl(implicit conf: Config): (Job => Boolean, Job => Boolean) =
    (conf.start, conf.end) match {
      case (Some(start), Some(end)) => (
        isBetween(start to end),
        startedAndEndedBetween(start to end)
      )

      case (Some(start), None) => (
        ended after start,
        started after start
      )

      case (None, Some(end)) => (
        started before end,
        ended before end
      )

      case (None, None) =>
        (allJobs, allJobs)
    }

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    // TODO cache (although once per file is ok)
    val categoryOf: Job => String = categoryF
    val (incl, excl) = inclexcl

    stream map { job =>
      val category = categoryOf(job)
      val cputime = job.res.cputime

      val data = if (excl(job)) {
        Base(cputime, cputime)
      } else if (incl(job)) {
        Base(inclusive = cputime)
      } else {
        // TODO don't bother with this
        // (although, surprisingly, this is faster than flatMap)
        Base.empty
      }

      Map(category -> data)
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    val small = data forall {
      case (_, a) =>
        a.inclusive === a.exclusive
    }

    if (small) {
      val reduced: Seq[(String, Double)] =
        data.mapValues(_.inclusive).sortBy(_._2)

      val all: (String, Double) =
        "all".localized -> reduced.foldLeft(0.0)((acc, x) => acc + x._2)

      val pretty: Iterable[Seq[String]] = for {
        (project, value) <- (reduced :+ all)
      } yield {
        Seq(s"$project", f"$value%.0f")
      }

      val table = Table("Category", "CPU Time")
      table.alignments(1) = Alignment.Right
      for (row <- pretty) {
        table.rows += row
      }
      table.print()
    } else {
      val sorted: Seq[Grouped] = data.sortBy(_._2.inclusive)

      val all: Grouped =
        "all".localized -> data.values.foldLeft(Base.empty)(_ |+| _)

      val pretty: Iterable[Seq[String]] = for {
        (project, Base(incl, excl)) <- (sorted :+ all)
        if incl != 0 && excl != 0
      } yield {
        val mean = (incl + excl) / 2
        Seq(s"$project", f"$excl%.0f", f"$incl%.0f", f"$mean%.0f")
      }

      val table = Table("Category", "Within Boundaries", "Beyond Boundaries", "Mean")
      table.alignments(1) = Alignment.Right
      table.alignments(2) = Alignment.Right
      table.alignments(3) = Alignment.Right
      for (row <- pretty) {
        table.rows += row
      }
      table.print()
    }
  }

}
