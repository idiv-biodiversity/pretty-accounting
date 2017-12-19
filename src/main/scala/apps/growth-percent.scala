package grid

import language.higherKinds

import cats.Monoid
import cats.implicits._
import fs2._
import java.text.NumberFormat
import org.jfree.chart.LegendItem
import org.jfree.chart.LegendItemCollection
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.axis.NumberTickUnit
import org.jfree.chart.title.TextTitle
import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

// TODO localize
object `growth-percent` extends AccAppNG("pa-growth-percent") with Streamy {

  self =>

  import TimeConverter.jodaToJFreeMonth

  // --------------------------------------------------------------------------
  // data
  // --------------------------------------------------------------------------

  type Data = Map[String, Map[LocalDate, Double]]

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  final
  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    stream map { job =>
      val date = job.time.start.toLocalDate.withDayOfMonth(1)

      job.time.running match {
        case Right(running) =>
          val millis = running.millis * job.slots
          val project = job.acl.project.getOrElse("unknown".localized)
          Map(project -> Map(date -> millis))

        case Left(message) =>
          Console.err.println(
            s"""${self.name}: ignoring job: $message"""
          )
          Map()
      }
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    // TODO configurable output
    for ((project, chart) <- chartb(data)) {
      chart.saveAsPNG(s"/tmp/wallclock-growth-percent-${project}.png", conf.resolution)
    }
  }

  // --------------------------------------------------------------------------
  // theoretical maximum
  // --------------------------------------------------------------------------

  lazy val purchasedAndIntegratedCPUs: Map[String, Double] = Map ( // TODO conf
    "idiv" -> 1312.0,
    "ufz" -> 1232.0
  ).withDefaultValue(0.0)

  def theoreticalMaxCPUSecondsPerMonth(project: String, date: LocalDate): Option[Double] =
    purchasedAndIntegratedCPUs.get(project).map(_ * 60 * 60 * 24 * date.dayOfMonth.getMaximumValue * 1000)

  // --------------------------------------------------------------------------
  // chart generation
  // --------------------------------------------------------------------------

  /** Returns one chart per project. */
  def chartb(data: Data)(implicit conf: Config) = {
    val all: Map[LocalDate, Double] = {
      val x = for {
        (_, x) <- data
        (date, value) <- x
      } yield {
        Map(date -> value)
      }

      val M = Monoid[Map[LocalDate, Double]]

      x.foldLeft(M.empty)(M.combine)
    }

    val xdata: Data = for {
      (project, subdata) <- data
      if purchasedAndIntegratedCPUs.isDefinedAt(project)
    } yield {
      val data: Map[LocalDate, Double] = for {
        (date, value) <- subdata
        max = all(date) // theoreticalMaxCPUSecondsPerMonth(project, date)
      } yield {
        date -> value / max
      }

      project -> data
    }

    for {
      (project, data) <- xdata
    } yield {
      val wctime = data.toTimeSeries("resource usage")

      val chart = XYLineChart(wctime)
      chart.title = s"resource usage $project"

      // TODO make available in scala-chart
      val na = new NumberAxis("")
      na.setNumberFormatOverride(NumberFormat.getPercentInstance)
      // TODO make available in scala-chart
      chart.plot.peer.setRangeAxis(na)

      regression(chart.plot.getDataset, "growth", series = 0) match {
        case Right(x) =>
          // TODO make available in scala-chart
          val regressions = new XYSeriesCollection()
          regressions.addSeries(new XYSeries("dummy"))

          chart.plot.setDataset(1, regressions)

          val (regs, subtitles) = x
          regs foreach regressions.addSeries

          for (subtitle <- subtitles) {
            // TODO make available in scala-chart
            chart.subtitles += new TextTitle(subtitle)
          }

          // TODO make available in scala-chart
          val l = chart.plot.getLegendItems.iterator.asInstanceOf[java.util.Iterator[LegendItem]].asScala.filterNot(_.getLabel.startsWith("dummy"))
          chart.plot.setFixedLegendItems(l.toLegendItemCollection)

        case Left(message) if conf.verbose =>
          Console.err.println(s"""[${self.name}] [$project] $message""")
      }

      project -> chart
    }
  }

  // --------------------------------------------------------------------------
  // chart utilities
  // --------------------------------------------------------------------------

  // TODO make available in scala-chart
  def DurationFormat: NumberFormat = new NumberFormat {
    def format(v: Long, b: StringBuffer, p: java.text.FieldPosition): StringBuffer =
      format(v.toDouble, b, p)

    def format(v: Double, b: StringBuffer, p: java.text.FieldPosition): StringBuffer = {
      val days = (v / 86400).toLong

      if (days > 0)
        b.append(days)

      b
    }

    def parse(a: String, b: java.text.ParsePosition): Number = ???
  }

  // TODO make available in scala-chart
  def DurationAxis(name: String) = {
    val a = new NumberAxis(name)
    a.setNumberFormatOverride(DurationFormat)
    a.setTickUnit(new NumberTickUnit(432000000.0)) // 5000 days
    a
  }

  // TODO make available in scala-chart
  implicit class RichLegendItems[CC[X] <: GenTraversableOnce[X]](self: CC[LegendItem]) {
    def toLegendItemCollection: LegendItemCollection = {
      val lic = new LegendItemCollection
      for (li <- self)
        lic add li
      lic
    }
  }

  // TODO make available in scala-chart
  def regression(data: org.jfree.data.xy.XYDataset, name: String, series: Int): Either[String, (List[XYSeries], List[String])] = {
    if (series >= data.getSeriesCount) {
      Left(s"dataset with ${data.getSeriesCount} series' does not contain series number ${series}")
    } else if (data.getItemCount(series) < 2) {
      val key = data.getSeriesKey(series)
      Left(s"series ${key} contains only ${data.getItemCount(series)} items, that's too few for a regression")
    } else {
      val Array(intercept, slope) = org.jfree.data.statistics.Regression.getOLSRegression(data, series)

      val yearlyGrowth = slope * 1000 * 60 * 60 * 24 * 365

      val g = (1 - intercept) / slope

      val regLine = new org.jfree.data.function.LineFunction2D(intercept, slope)

      val from = data.getXValue(series, 0)
      val to = data.getXValue(series, data.getItemCount(series) - 1)

      val linear = org.jfree.data.general.DatasetUtilities.sampleFunction2DToSeries(
        regLine,
        from,
        to,
        100,
        name
      )

      Right(List(linear) -> List(
        s"yearly growth: ${(yearlyGrowth * 10000).round / 100}%"
      ))
    }
  }

}
