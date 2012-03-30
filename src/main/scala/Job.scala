package grid

import org.scala_tools.time.Imports._

object Job {

  object JobId {
    def apply(s: String, t: String): JobId = {
      val parts = s split ":"
      JobId(id = parts(1).toInt, task = t.toInt, name = parts(0))
    }
  }

  case class JobId(id: Int, task: Int, name: String)

  object User {
    def apply(s: String): User = {
      val parts = s split ":"
      User(uid = parts(1), gid = parts(0))
    }
  }

  case class User(uid: String, gid: String)

  object Time {
    def apply(s: String): Time = {
      val parts = s split ":" map { _.toLong * 1000L } map { _.toDateTime }
      Time(submission = parts(0), start = parts(1), end = parts(2))
    }
  }

  case class Time(submission: DateTime, start: DateTime, end: DateTime)

  object Status {
    def apply(s: String): Status = {
      val parts = s split ":" map { _ toInt }
      Status(grid = parts(0), script = parts(1))
    }
  }

  case class Status(grid: Int, script: Int) {
    def successful = grid == 0 && script == 0
    def failed = ! successful
  }

  object ResourceUsage {
    def apply(s: String): ResourceUsage = {
      val parts = s split ":"
      ResourceUsage(
        wallclock = parts(0).toLong,
        utime     = parts(1),
        stime     = parts(2),
        maxrss    = parts(3),
        ixrss     = parts(4),
        ismrss    = parts(5),
        idrss     = parts(6),
        isrss     = parts(7),
        minflt    = parts(8),
        majflt    = parts(9),
        nswap     = parts(10),
        inblock   = parts(11),
        oublock   = parts(12),
        msgsnd    = parts(13),
        msgrcv    = parts(14),
        nsignals  = parts(15),
        nvcsw     = parts(16),
        nivcsw    = parts(17)
      )
    }
  }

  /**
    *
    * @param  wallclock  wallclock time in seconds
    */
  case class ResourceUsage (
      wallclock: Long,
      utime: String,
      stime: String,
      maxrss: String,
      ixrss: String,
      ismrss: String,
      idrss: String,
      isrss: String,
      minflt: String,
      majflt: String,
      nswap: String,
      inblock: String,
      oublock: String,
      msgsnd: String,
      msgrcv: String,
      nsignals: String,
      nvcsw: String,
      nivcsw: String
    )

  object Acl {
    def apply(s: String): Acl = {
      val parts = s split ":"
      Acl(department = parts(1), project = parts(0))
    }
  }

  case class Acl(department: String, project: String)

  object Reservation {
    def apply(s: String): Reservation = {
      val parts = s split ":"
      Reservation(id = parts(0).toInt, submission = parts(1).toLong)
    }
  }

  case class Reservation(id: Int, submission: Long)

}

import Job._

case class Job (
    queue: Option[String],
    node: Option[String],
    user: User,
    jobId: JobId,
    account: String,
    priority: Double,
    time: Time,
    status: Status,
    resourceUsage: ResourceUsage,
    acl: Acl,
    parallelEnvironment: Option[String],
    slots: Int,
    resourceRequest: Option[String],
    parallelTaskId: Option[Int],
    reservation: Reservation
  )
