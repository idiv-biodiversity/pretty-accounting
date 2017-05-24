package grid

import cats.Eq
import cats.implicits._

// TODO write and use pretty table CLI output function that automatically determines column
// width in external library

object CLI extends CLI

trait CLI {

  sealed trait Alignment
  object Alignment {
    object Left extends Alignment
    object Right extends Alignment

    implicit val AlignmentEq: Eq[Alignment] =
      Eq.fromUniversalEquals
  }

  /** Pretty prints a table on the console.
    *
    * Rows, header and align must all have the same size.
    *
    * If rows is empty, nothing happens.
    */
  def printTable(rows: Seq[Seq[String]], padding: Int = 1)(header: String*)(alignments: Alignment*): Unit =
    if (rows.size > 0) {
      val size = header.size

      require(rows.forall(_.size === size))
      require(alignments.size === size)
      require(padding >= 0)

      object table {
        val data: Seq[Seq[String]] = Seq(header) ++ rows // TODO should work without
        val padding = 1
        object size {
          val data: Seq[Int] = for (i <- header.indices) yield // TODO don't rely on indices
            table.data.map(_(i).size).max
          val padded: Seq[Int] = if (data.size > 2) {
            val p = data.map(_ + padding * 2)
            // TODO find better solution than using updated
            p.updated(0, p.head - padding).updated(p.size - 1, p.last - padding)
          } else {
            data.map(_ + padding)
          }
        }
      }

      def printRow(row: Seq[String], align: Boolean): String = {
        (row.zipWithIndex zip table.size.data).map({
          case ((cell, index), size) =>
            val l = cell.length

            if (align && alignments(index) === Alignment.Right) {
              s"${" " * (size - l)}$cell"
            } else {
              s"$cell${" " * (size - l)}"
            }
        }).mkString(" | ")
      }

      // print header
      println(printRow(header, align = true))

      // print border between header and body
      println(table.size.padded.map("-" * _).mkString("|"))

      // print body
      for (row <- rows) {
        println(printRow(row, align = true))
      }
    }

}
