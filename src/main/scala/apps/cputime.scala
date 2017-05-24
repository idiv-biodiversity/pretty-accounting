package grid

import cats.Monoid
import cats.implicits._

import fs2._
import fs2.interop.cats._

import scalax.cli.Table
import scalax.cli.Table.Alignment

// TODO localize
object cputime extends App with Accounting {

  Config.parser(name = "pa-cputime").parse(args, Config.empty) match {

    case Some(conf) =>
      val (threads, strategy) = conf.strategy
      implicit val S = strategy

      val (incl, excl) = (conf.start, conf.end) match {
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

      val streams: Stream[Task, Stream[Task, GroupedData]] =
        Stream(conf.accountingFiles: _*) map { file =>
          val proto = raw(file).filter(realJob)

          conf.mapWithProgress(proto, file) { job =>
            val category = categoryOf(job)
            val cputime = job.res.cputime

            val data = if (excl(job)) {
              Data(cputime, cputime)
            } else if (incl(job)) {
              Data(inclusive = cputime)
            } else {
              // TODO don't bother with this
              // (although this is faster than flatMap)
              Data.empty
            }

            category -> data
          }
        }

      val stream: Stream[Task, GroupedData] =
        fs2.concurrent.join(maxOpen = threads)(streams)

      val data: Map[String, Data] =
        stream.runFoldMap({ a =>
          val (project, data) = a
          Map(project -> data)
        }).unsafeRun

      val small = data forall {
        case (_, a) =>
          a.inclusive === a.exclusive
      }

      if (small) {
        val reduced: Seq[(String, Double)] =
          data.mapValues(_.inclusive).sortBy(_._2)

        val all: (String, Double) =
          "all" -> reduced.foldLeft(0.0)((acc, x) => acc + x._2)

        val pretty: Iterable[Seq[String]] = for {
          (project, value) <- (reduced :+ all)
        } yield {
          Seq(s"$project", f"$value%.0f")
        }

        val table = Table("Project", "CPU Time")
        table.alignments(1) = Alignment.Right
        for (row <- pretty) {
          table.rows += row
        }
        table.print()
      } else {
        val sorted: Seq[GroupedData] = data.sortBy(_._2.inclusive)

        val all: GroupedData =
          "all" -> data.values.foldLeft(Data.empty)(_ |+| _)

        val pretty: Iterable[Seq[String]] = for {
          (project, Data(incl, excl)) <- (sorted :+ all)
          if incl != 0 && excl != 0
        } yield {
          val mean = (incl + excl) / 2
          Seq(s"$project", f"$excl%.0f", f"$incl%.0f", f"$mean%.0f")
        }

        val table = Table("Project", "Within Boundaries", "Beyond Boundaries", "Mean")
        table.alignments(1) = Alignment.Right
        table.alignments(2) = Alignment.Right
        table.alignments(3) = Alignment.Right
        for (row <- pretty) {
          table.rows += row
        }
        table.print()
      }

    case None =>
      // command line arguments are wrong, help will already have been printed
      sys.exit(1)
  }

  case class Data(inclusive: Double = 0.0, exclusive: Double = 0.0)

  object Data {
    val empty = Data()
    implicit val DataMonoid: Monoid[Data] = new Monoid[Data] {
      override def empty: Data = Data.empty
      override def combine(x: Data, y: Data): Data =
        Data(x.inclusive + y.inclusive, x.exclusive + y.exclusive)
    }
  }

  type GroupedData = (String, Data)

}
