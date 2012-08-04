import collection._

package object grid extends org.scala_tools.time.Imports {

  implicit def coll2sortable[A](coll: GenTraversableOnce[A]) = new CollWrapper(coll)

  class CollWrapper[A](coll: GenTraversableOnce[A]) {

    def sortWith(lt: (A, A) ⇒ Boolean): List[A] = coll.toList.sortWith(lt)

    def sortBy[B](f: A ⇒ B)(implicit ord: Ordering[B]): List[A] = coll.toList.sortBy(f)(ord)

    def sorted[B >: A](implicit ord: Ordering[B]): List[A] = coll.toList.sorted(ord)

  }

  implicit def mapwrap[A,B](m: GenMap[A,B]) = new MapWrapper(m)

  class MapWrapper[A,B](m: GenMap[A,B]) {
    def mapValues[C](f: B ⇒ C): GenMap[A,C] = m map { x ⇒ x._1 → f(x._2) }
  }

  // -------------------------------------------------------------------
  // localization string wrapper
  // -------------------------------------------------------------------

  /** Returns an enriched `String` that provides additional methods for localization. */
  implicit def string2localized(s: String): BundleString = new BundleString(s)

  class BundleString(s: String) {
    def localized = try {
      java.util.ResourceBundle getBundle "PABundle" getString s
    } catch {
      case _ ⇒ s
    }
  }

}
