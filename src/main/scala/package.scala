import language.implicitConversions

import scala.collection.GenTraversableOnce

package object grid extends org.scala_tools.time.Imports {

  implicit class RichCollection[A](coll: GenTraversableOnce[A]) {
    def sortWith(lt: (A, A) ⇒ Boolean): List[A] = coll.toList.sortWith(lt)
    def sortBy[B](f: A ⇒ B)(implicit ord: Ordering[B]): List[A] = coll.toList.sortBy(f)(ord)
    def sorted[B >: A](implicit ord: Ordering[B]): List[A] = coll.toList.sorted(ord)
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
