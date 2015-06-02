import scala.collection.GenTraversableOnce
import org.jfree.data.time.RegularTimePeriod
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.{ Minute => JMinute }

package object grid extends com.github.nscala_time.time.Imports {

  implicit class RichCollection[A](coll: GenTraversableOnce[A]) {
    def mapValue[V](v: V): Map[A,V] = {
      val b = Map.newBuilder[A,V]
      coll foreach { k => b += ((k,v)) }
      b.result()
    }

    def sortWith(lt: (A, A) ⇒ Boolean): List[A] = coll.toList.sortWith(lt)
    def sortBy[B](f: A ⇒ B)(implicit ord: Ordering[B]): List[A] = coll.toList.sortBy(f)(ord)
    def sorted[B >: A](implicit ord: Ordering[B]): List[A] = coll.toList.sorted(ord)
  }

  implicit class RichCollectionOfTuple2s[A,B](trav: GenTraversableOnce[(A,B)]) {
    def toMinuteTimeSeries(name: Comparable[_] = "")(implicit eva: A => RegularTimePeriod, numb: Numeric[B]): TimeSeries = {
      trav.foldLeft(new TimeSeries(name, classOf[JMinute])) { case (series,(time,value)) =>
        series.add(time, numb.toDouble(value), false)
        series
      }
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

}
