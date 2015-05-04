package grid

import language.implicitConversions

object RichTime extends RichTime

trait RichTime {

  /** Returns the interval from the start of this year up to now. */
  def thisYear: Interval = {
    val now = DateTime.now
    now.withDayOfYear(1).withMillisOfDay(0) to now
  }

  implicit class RichGridDateTime(self: DateTime) {
    def getQuarterOfYear: String = self.getMonthOfYear match {
      case q1 if ( 1 <= q1) && (q1 <=  3) ⇒ "Q1"
      case q2 if ( 4 <= q2) && (q2 <=  6) ⇒ "Q2"
      case q3 if ( 7 <= q3) && (q3 <=  9) ⇒ "Q3"
      case q4 if (10 <= q4) && (q4 <= 12) ⇒ "Q4"
    }
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
