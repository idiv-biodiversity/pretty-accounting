package grid

object Accounting extends Accounting
object TypeImports extends TypeImports
object StaticImports extends StaticImports
object Implicits extends Implicits
object FileImplicits extends FileImplicits

trait Accounting extends TypeImports with StaticImports
  with Parsing with Filtering with Efficiency with RichCharting with RichJobs
  with Implicits {
}

trait Implicits extends FileImplicits

trait FileImplicits {
  implicit def string2file(s: String) = new java.io.File(s)
}

trait TypeImports {
  type GenIterable[A]     = scala.collection.GenIterable[A]
  type GenMap[A,B]        = scala.collection.GenMap[A,B]
  type GenSeq[A]          = scala.collection.GenSeq[A]

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
  def fileSeparator = java.io.File.separator
}
