package grid

object Internationalization extends Internationalization

trait Internationalization {

  implicit class BundleString(s: String) {
    def localized = try {
      java.util.ResourceBundle getBundle "PABundle" getString s
    } catch {
      case e: Exception ⇒
        Console.err.println(s"""[warn] no i18n for "$s" due to: ${e.getMessage}""")
        s
    }
  }

}
