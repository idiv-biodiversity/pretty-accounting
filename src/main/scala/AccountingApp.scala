package grid

import cats._
import fs2._

abstract class AccAppNG(val name: String) extends App with Accounting {

  def accmain(implicit conf: Config): Unit

  Config.parser(name).parse(args, Config.empty) match {
    case Some(conf) =>
      accmain(conf)

    case None =>
      sys.exit(1)
  }

}

trait Streamy {

  self: AccAppNG =>

  final type Path = java.nio.file.Path
  type Data

  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data]

  final
  override
  def accmain(implicit conf: Config): Unit = {
    val (threads, strategy) = conf.strategy
    implicit val S = strategy

    val streams: Stream[Task, Stream[Task, Data]] =
      Stream(conf.accountingFiles: _*) map { file =>
        f(raw(file) filter combined)
      }

    val stream: Stream[Task, Data] =
      concurrent.join(maxOpen = threads)(streams)

    r(stream)
  }

  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit

}

trait Tabular {

  self: AccAppNG with Streamy =>

  type A = String
  type B
  type Data = Map[A, B]
  type Grouped = (A, B)

  implicit def MB: Monoid[B]
  implicit def MD: Monoid[Data]
  implicit def OB: Ordering[B]

  final
  def o(data: Data): Option[Iterable[Grouped]] =
    data.size match {
      case 0 =>
        Console.err.println("no data") // TODO localize
        None

      case 1 =>
        Some(data)

      case _ =>
        val sorted: Seq[Grouped] =
          data.sortBy(_._2)

        val all: Grouped =
          "all".localized -> sorted.foldLeft(MB.empty)((acc, x) => MB.combine(acc, x._2))

        Some(sorted :+ all)
    }

  def p(data: Iterable[Grouped]): Unit

  final
  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()
    val result = o(data)

    for (data <- result)
      p(data)
  }

}

trait Charty {

  self: AccAppNG with Streamy =>

  type Chart = scalax.chart.Chart

  def c(data: Data): Chart

  final
  def o(chart: Chart)(implicit conf: Config): Unit = {
    chart.saveAsPNG(s"${self.name}.png", conf.resolution)
  }

}
