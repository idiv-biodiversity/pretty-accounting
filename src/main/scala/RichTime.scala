package grid

import language.implicitConversions

object RichTime extends RichTime

trait RichTime {

  /** Returns the interval from the start of this year up to now. */
  def thisYear: Interval = {
    val now = DateTime.now
    now.withDayOfYear(1).withMillisOfDay(0) to now
  }

  implicit class RichDate(self: DateTime) {
    def getQuarterOfYear: String = quarter

    def quarter: String = self.getMonthOfYear match {
      case q1 if ( 1 <= q1) && (q1 <=  3) ⇒ "Q1"
      case q2 if ( 4 <= q2) && (q2 <=  6) ⇒ "Q2"
      case q3 if ( 7 <= q3) && (q3 <=  9) ⇒ "Q3"
      case q4 if (10 <= q4) && (q4 <= 12) ⇒ "Q4"
    }
  }

  implicit class RichInterval(self: Interval) {
    import org.joda.time.ReadablePeriod

    def by(d: ReadablePeriod) = {
      val coll = collection.mutable.ListBuffer[DateTime]()

      var x = self.start
      while (x < self.end) {
        coll += x
        x += d
      }

      coll.toList
    }
  }

  implicit class RichString(self: String) {
    def toLocalDate = new LocalDate(self)
    def toDateTime  = new DateTime(self)

    def toLocalDateOption = try {
      Some(toLocalDate)
    } catch {
      case e: IllegalArgumentException ⇒ None
    }

    def toDateTimeOption = try {
      Some(toDateTime)
    } catch {
      case e: IllegalArgumentException ⇒ None
    }
  }

  implicit val DateTimeOrdering: Ordering[DateTime] = new Ordering[DateTime] {
    def compare(a: DateTime, b: DateTime) = a.compareTo(b)
  }

  implicit val LocalDateOrdering: Ordering[LocalDate] = new Ordering[LocalDate] {
    def compare(a: LocalDate, b: LocalDate) = a.compareTo(b)
  }

  // -----------------------------------------------------------------------------------------------
  // joda to jfreechart conversions
  // -----------------------------------------------------------------------------------------------

  import org.jfree.data.time.{
    Minute ⇒ JMinute,
    Day ⇒ JDay,
    SimpleTimePeriod
  }

  implicit def joda2jfreeminute(d: DateTime): JMinute = new JMinute(d.toDate)

  implicit def joda2jfreeday(d: LocalDate): JDay = new JDay(d.toDateTimeAtStartOfDay.toDate)

  implicit def interval2timeperiod(i: Interval): SimpleTimePeriod =
    new SimpleTimePeriod(i.start.toDate, i.end.toDate)

}
