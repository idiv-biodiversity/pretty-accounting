package grid

import java.io._

import org.jfree.chart.ChartFactory._
import org.jfree.chart.ChartUtilities._
import org.jfree.chart.plot.PlotOrientation._
import org.jfree.data.category._
import org.jfree.data.time._

object RichCharting extends RichCharting

trait RichCharting extends RichTime with TypeImports with StaticImports {

  // -------------------------------------------------------------------
  // implicit conversions
  // -------------------------------------------------------------------

  implicit def joda2jfreeminute(d: DateTime): Minute = new Minute(d.toDate)

  implicit def joda2jfreeday(d: LocalDate): Day = new Day(d.toString.toDateTime.toDate)

  implicit def interval2timeperiod(i: Interval): SimpleTimePeriod =
    new SimpleTimePeriod(i.start.toDate, i.end.toDate)

  implicit def toTimeSeries[A <% Number](it: GenIterable[(DateTime,A)]): TimeSeries = {
    val series = new TimeSeries("")
    it.seq foreach {
      case (t,v) => series.add(t,v)
    }
    series
  }

  implicit def toTimePeriodValues[A <% Number](it: GenIterable[(Interval,A)]): TimePeriodValues = {
    val series = new TimePeriodValues("")
    it.seq foreach {
      case (t,v) => series.add(t,v)
    }
    series
  }

  implicit def toTimePeriodValuesCollection[A <% Number](it: GenIterable[(Interval,A)]): TimePeriodValuesCollection =
    new TimePeriodValuesCollection(it)

  implicit def toTimeSeriesCollection[A <% Number](it: GenIterable[(DateTime,A)]): TimeSeriesCollection =
    new TimeSeriesCollection(it)

  implicit def toTimeTable[A <% Comparable[A], B <% Number]
      (m: Map[A,Iterable[(DateTime,B)]]): TimeTableXYDataset = {
    val dataset = new TimeTableXYDataset

    for {
      category      <-  m.keys
      (time,value)  <-  m(category)
    } dataset add (time, value, category, false)

    dataset
  }

  implicit def tuple2sToCategoryDataset[A <% Comparable[A],B <% Number]
      (it: GenIterable[(A,B)]): CategoryDataset = {
    val dataset = new DefaultCategoryDataset
    it.seq foreach {
      case (category,value) => dataset.addValue(value, category, "")
    }
    dataset
  }

  implicit def tuple3sToCategoryDataset[A <% Comparable[A],B <% Comparable[B],C <% Number]
      (it: GenIterable[(A,B,C)]): CategoryDataset = {
    val dataset = new DefaultCategoryDataset
    it.seq foreach {
      case (upperCat,lowerCat,value) => dataset.addValue(value, lowerCat, upperCat)
    }
    dataset
  }

  implicit def toCombinedDomainCategoryChart[A <% Comparable[A],B <% Comparable[B],C <% Comparable[C],D <% Number]
      (it: Iterable[(A,B,C,D)]): JFreeChart = {
    val plot = new org.jfree.chart.plot.CombinedDomainCategoryPlot

    it groupBy { _._1 } mapValues { coll =>
      tuple3sToCategoryDataset(coll map { t => (t._2,t._3,t._4) })
    } foreach { x =>
      val (cat,dataset) = x
      plot.add(createLabelledBarChart(dataset,cat.toString).getPlot.asInstanceOf[CategoryPlot])
    }

    new JFreeChart(plot)
  }

  // -------------------------------------------------------------------
  // chart creation wrappers
  // -------------------------------------------------------------------

  def createAreaChart(implicit dataset: XYDataset, title: String = "") = {
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

    if (dataset.isInstanceOf[TimePeriodValuesCollection] ||
        dataset.isInstanceOf[TimeSeriesCollection])
      chart.getXYPlot.setDomainAxis(new DateAxis)

    chart
  }

  def createStackedAreaChart(implicit dataset: TableXYDataset, title: String = "") = {
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

    if (dataset.isInstanceOf[TimeTableXYDataset])
      chart.getXYPlot.setDomainAxis(new DateAxis)

    chart
  }

  def createLineChart(implicit dataset: XYDataset, title: String = "") = {
    val chart = createXYLineChart (
      /* title       = */ title,
      /* xAxisLabel  = */ "",
      /* yAxisLabel  = */ "",
      /* dataset     = */ dataset,
      /* orientation = */ VERTICAL,
      /* legend      = */ true,
      /* tooltips    = */ false,
      /* urls        = */ false
    )

    if (dataset.isInstanceOf[TimePeriodValuesCollection] ||
        dataset.isInstanceOf[TimeSeriesCollection])
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

  /** Implicitly enriches a JFreeChart. */
  implicit def enrichJFreeChart(chart: JFreeChart) = new RichChart(chart)

  /** Enriched JFreeChart. */
  class RichChart(chart: JFreeChart) {

    /** Shows the chart in a window. */
    def show = onEDT {
      new ChartFrame("", chart, true) setVisible true
    }

    /** Saves the chart.
      *
      * @param ext extension of the file / output type, currently supported are PNG, JPEG and PDF
      * @param output file to where will be written
      * @param dim dimension / geometry / width x height of the output
      */
    def saveAs(ext: String)
              (implicit output: File,
                        dim: Pair[Int,Int]): Unit = ext.toLowerCase match {
      case "pdf"          => saveAsPDF
      case "png"          => saveAsPNG
      case "jpg" | "jpeg" => saveAsJPEG
      case _              => sys error """Extension "%s" is not supported.""".format(ext)
    }

    // -------------------------------------------------------------------
    // save wrappers
    // -------------------------------------------------------------------

    /** Saves the chart as a PNG image.
      *
      * @param output file to where will be written
      * @param dim dimension / geometry / width x height of the output
      */
    def saveAsPNG(implicit output: File, dim: Pair[Int,Int]) {
      saveChartAsPNG(output, chart, dim._1, dim._2)
    }

    /** Saves the chart as a JPEG image.
      *
      * @param output file to where will be written
      * @param dim dimension / geometry / width x height of the output
      */
    def saveAsJPEG(implicit output: File, dim: Pair[Int,Int]) {
      saveChartAsJPEG(output, chart, dim._1, dim._2)
    }

    // -------------------------------------------------------------------
    // export to pdf
    // -------------------------------------------------------------------

    import java.awt.geom._
    import com.lowagie.text._
    import com.lowagie.text.pdf._

    /** Saves the chart as a PDF.
      *
      * @param output file to where will be written
      * @param dim dimension / geometry / width x height of the output
      * @param fontMapper handles mappings between Java AWT Fonts and PDF fonts
      */
    def saveAsPDF(implicit output: File, dim: Pair[Int,Int], fontMapper: FontMapper = new DefaultFontMapper) {
      implicit val os = new BufferedOutputStream(new FileOutputStream(output))

      try {
        writeAsPDF
      } finally {
        os.close()
      }
    }

    /** Writes the chart as a PDF.
      *
      * @param os stream to where will be written
      * @param dim dimension / geometry / width x height of the output
      * @param fontMapper handles mappings between Java AWT Fonts and PDF fonts
      */
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
