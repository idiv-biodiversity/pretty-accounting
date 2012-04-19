package grid

import org.jfree.chart.ChartFactory._
import org.jfree.chart.plot.PlotOrientation._

object RichCharting extends RichCharting

trait RichCharting extends TypeImports with StaticImports {
  implicit def joda2jfreeminute(d: DateTime) = new org.jfree.data.time.Minute(d.toDate)

  implicit def timeslots2timeseries[A <% Number](it: Iterable[(DateTime,A)]) = {
    val dataset = new TimeSeries("")
    it foreach { kv =>
      dataset.add(kv._1,kv._2)
    }
    dataset
  }

  implicit def groupedtimeslots2timetable[A <% Comparable[A], B <% Number](groups: Map[A,Iterable[(DateTime,B)]]) = {
    val dataset = new TimeTableXYDataset()

    for {
      group         <-  groups.keys
      (time,value)  <-  groups(group)
    } dataset add (time, value, group, false)

    dataset
  }

  def show(chart: JFreeChart)(implicit title: String = "") = onEDT {
    new ChartFrame(title, chart, true) setVisible true
  }

  def createTimeSeriesAreaChart(implicit dataset: XYDataset, title: String = "") = {
    val chart = createXYAreaChart (
      /* title       = */ title,
      /* xAxisLabel  = */ "",
      /* yAxisLabel  = */ "",
      /* dataset     = */ dataset,
      /* orientation = */ VERTICAL,
      /* legend      = */ true,
      /* tooltips    = */ false,
      /* urls        = */ false
    )
    chart.getXYPlot.setDomainAxis(new DateAxis)
    chart
  }

  def createTimeSeriesStackedAreaChart(implicit dataset: TableXYDataset, title: String = "") = {
    val chart = createStackedXYAreaChart (
      /* title       = */ title,
      /* xAxisLabel  = */ "",
      /* yAxisLabel  = */ "",
      /* dataset     = */ dataset,
      /* orientation = */ VERTICAL,
      /* legend      = */ true,
      /* tooltips    = */ false,
      /* urls        = */ false
    )
    chart.getXYPlot.setDomainAxis(new DateAxis)
    chart
  }

  implicit def pimpedJFreeChart(chart: JFreeChart) = new JFreeChartPimp(chart)

  class JFreeChartPimp(chart: JFreeChart) {

    // -------------------------------------------------------------------
    // export to pdf
    // -------------------------------------------------------------------

    import java.awt.geom._
    import java.io._
    import com.lowagie.text._
    import com.lowagie.text.pdf._

    def saveAsPDF(file: File, width: Int, height: Int)
      (implicit fontMapper: FontMapper = new DefaultFontMapper) {
      val os = new BufferedOutputStream(new FileOutputStream(file))

      try {
        writeAsPDF(os, width, height, fontMapper)
      } finally {
        os.close()
      }
    }

    def writeAsPDF(os: OutputStream, width: Int, height: Int, fontMapper: FontMapper) {
      val pagesize = new Rectangle(width,height)
      val document = new Document(pagesize, 50, 50, 50, 50)

      try {
        val writer = PdfWriter.getInstance(document, os)
        document.open()

        val cb = writer.getDirectContent
        val tp = cb.createTemplate(width, height)
        val g2 = tp.createGraphics(width, height, fontMapper)
        val r2D = new Rectangle2D.Double(0, 0, width, height)

        chart.draw(g2, r2D)
        g2.dispose()
        cb.addTemplate(tp, 0, 0)
      } finally {
        document.close()
      }
    }

  }

}
