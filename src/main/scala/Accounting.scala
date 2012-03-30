package grid

object Accounting extends Accounting
object TypeImports extends TypeImports
object StaticImports extends StaticImports

trait Accounting extends TypeImports with StaticImports
  with Filtering with Efficiency with RichCharting with RichJobs
  with Implicits {

  def defaultAccountingFilePath =
    sys.env.getOrElse("SGE_ROOT", "/usr/local/sge") + "/default/common/accounting"

  def defaultAccountingFileLines =
    io.Source.fromFile(defaultAccountingFilePath).getLines.toIterable.par

  def load(implicit lines: GenIterable[String] = defaultAccountingFileLines) = lines collect {
    case AccountingEntry(job) => job
  }

  def filtered(implicit lines: GenIterable[String] = defaultAccountingFileLines) =
    load filter combined

  def linesNotMatching(implicit lines: GenIterable[String] = defaultAccountingFileLines) =
    lines filterNot { AccountingEntry.unapply(_).isDefined }
}

trait TypeImports {
  type GenIterable[A]     = scala.collection.GenIterable[A]
  type GenMap[A,B]        = scala.collection.GenMap[A,B]

  type ChartFrame         = org.jfree.chart.ChartFrame
  type JFreeChart         = org.jfree.chart.JFreeChart
  type DateAxis           = org.jfree.chart.axis.DateAxis
  type TimeSeries         = org.jfree.data.time.TimeSeries
  type TimeSeriesDataItem = org.jfree.data.time.TimeSeriesDataItem
  type TimeTableXYDataset = org.jfree.data.time.TimeTableXYDataset
  type TableXYDataset     = org.jfree.data.xy.TableXYDataset
  type XYDataset          = org.jfree.data.xy.XYDataset
}

trait StaticImports {
  def onEDT(f: => Unit) = scala.swing.Swing.onEDT(f)
}
