package grid

import language.higherKinds

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
object growth extends AccAppNG("pa-growth") with Streamy {

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
      val millis = job.time.running.millis * job.slots
      val project = job.acl.project.get

      Map(project -> Map(date -> millis))
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    data.mapValues(_.size) foreach println // TODO remove

    import TimeConverter.jodaToJFreeMonth

    val purchasedAndIntegratedCPUs: Map[String, Double] = Map ( // TODO conf
      "idiv" -> 1312.0,
      "ufz" -> 1232.0
    ).withDefaultValue(0.0)

    // TODO make dependent of month of year (28, 29, 30, 31)
    def theoreticalMaxCPUSecondsPerMonth(project: String) =
      purchasedAndIntegratedCPUs(project) * 60 * 60 * 24 * 30 * 1000

    def charta = {
      val dataset = data.map(kv => kv._1 -> kv._2.mapValues(_ / theoreticalMaxCPUSecondsPerMonth(kv._1))).toSeq.sortBy(_._1).toTimeSeriesCollection
      val nseries = dataset.getSeriesCount

      val chart = XYLineChart(dataset)
      chart.title = "resource usage"

      val na = new NumberAxis("")
      na.setNumberFormatOverride(NumberFormat.getPercentInstance)
      chart.plot.peer.setRangeAxis(na)

      // TODO make available in scala-chart
      // chart.plot.peer.setRangeAxis(DurationAxis("accumulated time in days"))

      // TODO make regression stuff in scala-chart
      val regressions = new XYSeriesCollection()

      // fill with number of dummy series to not use the same colors twice
      for (i <- 0 until nseries) {
        regressions.addSeries(new XYSeries(s"dummy-$i"))
      }

      for (i <- 0 until nseries) {
        val category = dataset.getSeriesKey(i).toString
        val name = s"$category growth"

        regression(dataset, name, series = i)._1 foreach regressions.addSeries
      }

      chart.plot.setDataset(1, regressions)

      // TODO make available in scala-chart
      val l = chart.plot.getLegendItems.iterator.asInstanceOf[java.util.Iterator[LegendItem]].asScala.filterNot(_.getLabel.startsWith("dummy-"))
      chart.plot.setFixedLegendItems(l.toLegendItemCollection)

      chart
    }

    def chartb = data map { kv =>
      val project = kv._1
      val data = kv._2

      val wctime = data.mapValues(_ / theoreticalMaxCPUSecondsPerMonth(project)).toTimeSeries("resource usage")

      val chart = XYLineChart(wctime)
      chart.title = s"resource usage $project"
      // TODO make available in scala-chart
      val na = new NumberAxis("")
      na.setNumberFormatOverride(NumberFormat.getPercentInstance)
      // TODO make available in scala-chart
      chart.plot.peer.setRangeAxis(na)

      // TODO make available in scala-chart
      val regressions = new XYSeriesCollection()
      regressions.addSeries(new XYSeries("dummy"))
      val (regs, subtitles) = regression(chart.plot.getDataset, "growth", series = 0)
      regs foreach regressions.addSeries
      chart.plot.setDataset(1, regressions)

      for (subtitle <- subtitles)
        // TODO make available in scala-chart
        chart.subtitles += new TextTitle(subtitle)

      // TODO make available in scala-chart
      val l = chart.plot.getLegendItems.iterator.asInstanceOf[java.util.Iterator[LegendItem]].asScala.filterNot(_.getLabel.startsWith("dummy"))
      chart.plot.setFixedLegendItems(l.toLegendItemCollection)

      project -> chart
    }

    charta.saveAsPNG("/tmp/wallclock.png", conf.resolution)

    for ((project, chart) <- chartb) {
      chart.saveAsPNG(s"/tmp/wallclock-$project-percent.png", conf.resolution)
    }

    // -----------------------------------------------------------------------------------------------
    // utilities
    // -----------------------------------------------------------------------------------------------

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
    def regression(data: org.jfree.data.xy.XYDataset, name: String, series: Int): (List[XYSeries], List[String]) = {
      val Array(intercept, slope) = org.jfree.data.statistics.Regression.getOLSRegression(data, series)

      val yearlyGrowth = slope * 1000 * 60 * 60 * 24 * 365

      val g = (1 - intercept) / slope

      val regLine = new org.jfree.data.function.LineFunction2D(intercept, slope)

      val from = data.getXValue(series, 0)
      val to = data.getXValue(series, data.getItemCount(0) - 1)

      val linear = org.jfree.data.general.DatasetUtilities.sampleFunction2DToSeries(
        regLine,
        from,
        to,
        100,
        name
      )

      List(linear) -> List(
        s"yearly growth: ${(yearlyGrowth * 10000).round / 100}%",
        s"reaching 100%: ${new java.util.Date(g.toLong)}"
      )
    }
  }

}
