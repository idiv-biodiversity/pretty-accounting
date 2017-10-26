package grid

import cats.implicits._
import fs2._

trait Slotty extends Streamy with Charty {

  self: AccAppNG =>

  type Data = Map[String, Map[DateTime, Int]]

  def cat(implicit conf: Config): Job => String

  final
  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    // TODO cache (although once per file is ok)
    val categoryOf: Job => String = cat

    stream map { job =>
      val category = categoryOf(job)
      val data = job.perMinute(_.slots)
      Map(category -> data)
    }
  }

  final
  override
  def c(data: Data): Chart = {
    import TimeConverter.jodaToJFreeMinute
    val chart = XYAreaChart.stacked(data.toTimeTable)
    chart
  }

  final
  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    val od: Option[Data] = data.size match {
      case 0 =>
        Console.err.println("no data") // TODO localize
        None

      case _ =>
        Some(data)
    }

    for (data <- od)
      o(c(data))
  }

}

object slots extends AccAppNG("pa-slots") with Slotty {
  override
  def cat(implicit conf: Config): Job => String =
    categoryF
}

object `slots-seq-vs-par` extends AccAppNG("pa-slots-seq-vs-par") with Slotty {
  override
  def cat(implicit conf: Config): Job => String =
    SeqVsPar
}

object `slots-per-queue` extends AccAppNG("pa-slots-per-queue") with Slotty {
  override
  def cat(implicit conf: Config): Job => String =
    job => job.queue.get
}
