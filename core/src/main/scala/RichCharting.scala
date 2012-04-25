package grid

import java.io._

import org.jfree.chart.ChartFactory._
import org.jfree.chart.ChartUtilities._
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

  def createLabelledBarChart(implicit dataset: CategoryDataset, title: String = "") = {
    val chart = createBarChart(
      /* title             = */ title,
      /* categoryAxisLabel = */ "",
      /* valueAxisLabel    = */ "",
      /* dataset           = */ dataset,
      /* orientation       = */ VERTICAL,
      /* legend            = */ true,
      /* tooltips          = */ false,
      /* urls              = */ false
    )

    val renderer = chart.getPlot.asInstanceOf[CategoryPlot].getRenderer
    val labelgen = new CategoryLabeller

    renderer.setBaseItemLabelsVisible(true)
    renderer.setBaseItemLabelGenerator(labelgen)

    chart
  }

  implicit def pimpedJFreeChart(chart: JFreeChart) = new JFreeChartPimp(chart)

  class JFreeChartPimp(chart: JFreeChart) {

    def saveAs(ext: String)(implicit output: File, dim: Pair[Int,Int]): Unit = ext.toLowerCase match {
      case "pdf"          => saveAsPDF
      case "png"          => saveAsPNG
      case "jpg" | "jpeg" => saveAsJPEG
      case _              => sys error ("""Extension "%s" is not supported.""" format ext)
    }

    // -------------------------------------------------------------------
    // save wrappers
    // -------------------------------------------------------------------

    def saveAsPNG(implicit output: File, dim: Pair[Int,Int]) {
      saveChartAsPNG(output, chart, dim._1, dim._2)
    }

    def saveAsJPEG(implicit output: File, dim: Pair[Int,Int]) {
      saveChartAsJPEG(output, chart, dim._1, dim._2)
    }

    // -------------------------------------------------------------------
    // export to pdf
    // -------------------------------------------------------------------

    import java.awt.geom._
    import com.lowagie.text._
    import com.lowagie.text.pdf._

    def saveAsPDF(implicit output: File, dim: Pair[Int,Int], fontMapper: FontMapper = new DefaultFontMapper) {
      implicit val os = new BufferedOutputStream(new FileOutputStream(output))

      try {
        writeAsPDF
      } finally {
        os.close()
      }
    }

    def writeAsPDF(implicit os: OutputStream, dim: Pair[Int,Int], fontMapper: FontMapper) {
      val (width,height) = dim

      val pagesize = new Rectangle(width, height)
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
