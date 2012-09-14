package grid

import language.implicitConversions

object Accounting extends Accounting
object TypeImports extends TypeImports
object StaticImports extends StaticImports
object Implicits extends Implicits
object FileImplicits extends FileImplicits

trait Accounting extends TypeImports with StaticImports
  with Parsing with Filtering with RichJobs
  with org.sfree.chart.Charting
  with Implicits {
}

trait Implicits extends FileImplicits

trait FileImplicits {
  implicit def string2file(s: String) = new java.io.File(s)
}

trait TypeImports {
  type GenIterable[A] = scala.collection.GenIterable[A]
  type GenMap[A,B]    = scala.collection.GenMap[A,B]
  type GenSeq[A]      = scala.collection.GenSeq[A]
}

trait StaticImports {
  def fileSeparator = java.io.File.separator
}
