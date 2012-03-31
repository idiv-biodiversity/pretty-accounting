package grid

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
        wctime    = parts(0).toLong,
        utime     = parts(1).toDouble,
        stime     = parts(2).toDouble,
        maxrss    = parts(3).toDouble,
        ixrss     = parts(4).toLong,
        ismrss    = parts(5).toLong,
        idrss     = parts(6).toLong,
        isrss     = parts(7).toLong,
        minflt    = parts(8).toLong,
        majflt    = parts(9).toLong,
        nswap     = parts(10).toLong,
        inblock   = parts(11).toDouble,
        oublock   = parts(12).toLong,
        msgsnd    = parts(13).toLong,
        msgrcv    = parts(14).toLong,
        nsignals  = parts(15).toLong,
        nvcsw     = parts(16).toLong,
        nivcsw    = parts(17).toLong
      )
    }
  }

  /**
    *
    * @param  wctime      wallclock time in seconds
    * @param  utime       user time
    * @param  stime       system time
    * @param  maxrss      maximum resident set size
    * @param  ixrss       integral shared memory size
    * @param  ismrss      
    * @param  idrss       integral unshared data size
    * @param  isrss       integral unshared stack size
    * @param  minflt      page reclaims
    * @param  majflt      page faults
    * @param  nswap       swaps
    * @param  inblock     block input operations
    * @param  oublock     block output operations
    * @param  msgsnd      messages sent
    * @param  msgrcv      messages received
    * @param  nsignals    signals received
    * @param  nvcsw       voluntary context switches
    * @param  nivcsw      involuntary context switches
    */
  case class ResourceUsage (
      wctime: Long,
      utime: Double,
      stime: Double,
      maxrss: Double,
      ixrss: Long,
      ismrss: Long,
      idrss: Long,
      isrss: Long,
      minflt: Long,
      majflt: Long,
      nswap: Long,
      inblock: Double,
      oublock: Long,
      msgsnd: Long,
      msgrcv: Long,
      nsignals: Long,
      nvcsw: Long,
      nivcsw: Long
    ) {
    def cputime = utime + stime
  }

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
    res: ResourceUsage,
    acl: Acl,
    parallelEnvironment: Option[String],
    slots: Int,
    resReq: Option[String],
    parallelTaskId: Option[Int],
    reservation: Reservation
  )
