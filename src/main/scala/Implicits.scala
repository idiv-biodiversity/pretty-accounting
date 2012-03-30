package grid

object Implicits extends Implicits
object TimeImplicits extends TimeImplicits

trait Implicits extends TimeImplicits

trait TimeImplicits extends org.scala_tools.time.Imports {
  implicit def intervalpimp(interval: Interval) = new IntervalPimp(interval)

  class IntervalPimp(interval: Interval) {
    import org.joda.time.ReadablePeriod

    def by(d: ReadablePeriod) = {
      val coll = collection.mutable.ListBuffer[DateTime]()

      var x = interval.start
      while (x < interval.end) {
        coll += x
        x += d
      }

      coll.toList
    }
  }

  implicit val DateTimeOrdering = new Ordering[DateTime] {
    def compare(a: DateTime, b: DateTime) = a.compareTo(b)
  }

  implicit val LocalDateOrdering = new Ordering[LocalDate] {
    def compare(a: LocalDate, b: LocalDate) = a.compareTo(b)
  }
}
