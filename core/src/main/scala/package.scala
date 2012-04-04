package object grid extends org.scala_tools.time.Imports {
  def verbosef[A](f: String, a: A): A = {
    Console.err.println(f.format(a.toString))
    a
  }
}
