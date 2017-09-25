import scala.collection.GenTraversableOnce
import org.jfree.data.time.RegularTimePeriod
import org.jfree.data.time.TimeSeries
import org.joda.time.ReadablePeriod

package object grid extends com.github.nscala_time.time.Imports {

  implicit class RichCollection[A](val self: GenTraversableOnce[A]) extends AnyVal {
    def sortWith(lt: (A, A) ⇒ Boolean): List[A] = self.toList.sortWith(lt)
    def sortBy[B](f: A ⇒ B)(implicit ord: Ordering[B]): List[A] = self.toList.sortBy(f)(ord)
    def sorted[B >: A](implicit ord: Ordering[B]): List[A] = self.toList.sorted(ord)
  }

  implicit class RichCollectionOfTuple2s[A, B](val self: GenTraversableOnce[(A, B)]) extends AnyVal {
    def toMinuteTimeSeries(name: Comparable[_] = "")(implicit eva: A => RegularTimePeriod, numb: Numeric[B]): TimeSeries = {
      self.foldLeft(new TimeSeries(name)) { case (series,(time,value)) =>
        series.add(time, numb.toDouble(value), false)
        series
      }
    }
  }

  implicit class RichInterval(val self: Interval) extends AnyVal {
    def byWith[A](period: ReadablePeriod)(value: A): Map[DateTime, A] = {
      val builder = Map.newBuilder[DateTime, A]

      var x = self.getStart

      while (x <= self.getEnd) {
        builder += (x -> value)
        x += period
      }

      builder.result
    }
  }

  // ------------------------------------------------------------------------------------------------
  // localization string wrapper
  // ------------------------------------------------------------------------------------------------

  implicit class BundleString(s: String) {
    def localized = try {
      java.util.ResourceBundle getBundle "PABundle" getString s
    } catch {
      case _ : Exception ⇒ s
    }
  }

  object thread {
    def name: String = Thread.currentThread.getName
  }

}
