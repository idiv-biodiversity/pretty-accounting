package grid

import language.higherKinds

import java.text.NumberFormat

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

import cats._
import cats.implicits._
import fs2._
import fs2.interop.cats._
import org.jfree.chart.{LegendItem, LegendItemCollection}
import org.jfree.chart.axis.{NumberAxis, NumberTickUnit}
import org.jfree.chart.title.TextTitle

// TODO localize
object growth extends App with Accounting with RichTime {

  Config.parser(name = "pa-growth").parse(args, Config()) match {
    case Some(conf) =>
      import TimeConverter.jodaToJFreeMonth

      val (threads, strategy) = conf.strategy
      implicit val S = strategy

      val purchasedAndIntegratedCPUs = 1312.0 // TODO conf
      // TODO make def dependent of month of year (28, 29, 30, 31)
      val theoreticalMaxCPUSecondsPerMonth = purchasedAndIntegratedCPUs * 30 * 24 * 60 * 60

      val streams: Stream[Task, Stream[Task, GroupedData]] =
        Stream(conf.accountingFiles: _*) map { file =>
          val proto = raw(file).filter(job => realJob(job) && job.res.cputime > 0 && conf.startedBetween(job))

          conf.mapWithProgress(proto, file) { job =>
            val date = job.time.start.toLocalDate.withDayOfMonth(1)
            val data = Data(job.res.cputime, job.res.wctime * job.slots)

            date -> data
          }
        }

      val stream: Stream[Task, GroupedData] = fs2.concurrent.join(maxOpen = threads)(streams)

      val data: Map[LocalDate, Data] = stream.runFoldMap(a => Map(a._1 -> a._2)).unsafeRun

      def charta = {
        val cputime = data.mapValues(_.cputime).toTimeSeries("cputime")
        val wctime = data.mapValues(_.wctime).toTimeSeries("wallclock")

        val chart = XYLineChart(List(cputime, wctime))
        chart.title = "Resource Usage"
        // TODO make available in scala-chart
        chart.plot.peer.setRangeAxis(DurationAxis("accumulated time in days"))

        // TODO make available in scala-chart
        val regressions = new XYSeriesCollection()
        regressions.addSeries(new XYSeries("dummya"))
        regressions.addSeries(new XYSeries("dummyb"))
        regression(chart.plot.getDataset, "cputime growth", series = 0)._1 foreach regressions.addSeries
        regression(chart.plot.getDataset, "wallclock growth", series = 1)._1 foreach regressions.addSeries
        chart.plot.setDataset(1, regressions)

        // TODO make available in scala-chart
        val l = chart.plot.getLegendItems.iterator.asInstanceOf[java.util.Iterator[LegendItem]].asScala.filterNot(_.getLabel.startsWith("dummy"))
        chart.plot.setFixedLegendItems(l.toLegendItemCollection)

        chart
      }

      def chartb = {
        val wctime = data.mapValues(_.wctime / theoreticalMaxCPUSecondsPerMonth).toTimeSeries("resource usage")

        val chart = XYLineChart(wctime)
        chart.title = "Resource Usage"
        // TODO make available in scala-chart
        val na = new NumberAxis("usage in percent of theoretical maximum (purchased and fully integrated hardware)")
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

        chart
      }

      charta.saveAsPNG("/tmp/wallclock.png", resolution = 1920 -> 1080) // TODO conf
      chartb.saveAsPNG("/tmp/wallclock-percent.png", resolution = 1920 -> 1080) // TODO conf

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
      implicit class RichLegendItems[CC[X] <: GenTraversableOnce[X]](underlying: CC[LegendItem]) {
        def toLegendItemCollection: LegendItemCollection = {
          val lic = new LegendItemCollection
          for (li <- underlying)
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

    case None =>
      sys.exit(1)
  }

  case class Data(cputime: Double, wctime: Double) {
    def +(that: Data) = {
      Data(cputime = this.cputime + that.cputime, wctime = this.wctime + that.wctime)
    }
  }

  object Data {
    val empty = Data(0, 0)
    implicit val DataMonoid: Monoid[Data] = new Monoid[Data] {
      val empty = Data.empty
      def combine(x: Data, y: Data) = x + y
    }
  }

  type GroupedData = (LocalDate, Data)

}
