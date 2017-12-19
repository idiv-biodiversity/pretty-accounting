package grid

import cats.Monoid
import cats.implicits._
import fs2._

object `playground` extends AccAppNG("pa-playground") with Streamy {

  type Data = Map[String, Set[String]]

  // --------------------------------------------------------------------------
  // prog
  // --------------------------------------------------------------------------

  override
  def f(stream: Stream[Task, Job])(implicit conf: Config): Stream[Task, Data] = {
    stream map { job =>
      val uid = job.user.uid

      job.acl.department match {
        case dep if dep === "phydiv" || dep === "ess" || dep === "bzf" =>
          Map(dep -> Set(uid))

        case _ =>
          Map()
      }
    }
  }

  override
  def r(stream: Stream[Task, Data])(implicit conf: Config): Unit = {
    val data: Data = stream.runFoldMonoid.unsafeRun()

    for ((dep, list) <- data) {
      println(s"""$dep: ${list.mkString(", ")}""")
    }
  }

}
