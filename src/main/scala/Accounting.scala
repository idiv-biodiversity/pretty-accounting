package grid

import language.implicitConversions

object Accounting extends Accounting
trait Accounting
    extends Imports
    with Implicits
    with scalax.chart.module.Charting
    with Categorizing
    with Filtering
    with Streaming
    with RichTime
    with Internationalization

object Implicits extends Implicits
trait Implicits extends FileImplicits

object FileImplicits extends FileImplicits
trait FileImplicits {
  implicit def string2file(s: String) = new java.io.File(s)
}

object Imports extends Imports
trait Imports extends TypeImports with StaticImports

object TypeImports extends TypeImports
trait TypeImports {
  type GenIterable[A] = scala.collection.GenIterable[A]
  type GenMap[A,B]    = scala.collection.GenMap[A,B]
  type GenSeq[A]      = scala.collection.GenSeq[A]
}

object StaticImports extends StaticImports
trait StaticImports {
  def fileSeparator = java.io.File.separator
}
