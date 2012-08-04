package grid

object RichTime extends RichTime

trait RichTime {
  def thisYear = {
    val now = DateTime.now
    now.withDayOfYear(1).withMillisOfDay(0) to now
  }

  implicit def intervalpimp(interval: Interval) = new IntervalPimp(interval)
  implicit def string2richstring(s: String) = new RichString(s)

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

  class RichString(s: String) {
    def toLocalDate = new LocalDate(s)
    def toDateTime  = new DateTime(s)

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

  implicit val DateTimeOrdering = new Ordering[DateTime] {
    def compare(a: DateTime, b: DateTime) = a.compareTo(b)
  }

  implicit val LocalDateOrdering = new Ordering[LocalDate] {
    def compare(a: LocalDate, b: LocalDate) = a.compareTo(b)
  }

  // -----------------------------------------------------------------------------------------------
  // joda to jfreechart conversions
  // -----------------------------------------------------------------------------------------------

  import org.jfree.data.time.{Minute ⇒ JMinute, Day ⇒ JDay, SimpleTimePeriod}

  implicit def joda2jfreeminute(d: DateTime): JMinute = new JMinute(d.toDate)

  implicit def joda2jfreeday(d: LocalDate): JDay = new JDay(d.toDateTimeAtStartOfDay.toDate)

  implicit def interval2timeperiod(i: Interval): SimpleTimePeriod =
    new SimpleTimePeriod(i.start.toDate, i.end.toDate)

}
