package grid

import cats.Monoid
import cats.implicits._

import fs2._
import fs2.interop.cats._

// TODO localize
object cputime extends App with Accounting with CLI {

  Config.parser(name = "pa-cputime").parse(args, Config()) match {

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

        printTable(pretty.toSeq)( // TODO shouldn't need toSeq here
          header = "Project", "CPU Time"
        )(
          alignments = Alignment.Left, Alignment.Right
        )
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

        printTable(pretty.toSeq)( // TODO shouldn't need toSeq here
          header = "Project", "Within Boundaries", "Beyond Boundaries", "Mean"
        )(
          alignments = Alignment.Left,
                       Alignment.Right,
                       Alignment.Right,
                       Alignment.Right
        )
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
