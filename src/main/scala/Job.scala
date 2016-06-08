package grid

object Job {

  object JobId {
    def apply(s: String, t: String): JobId = {
      val parts = s split ":"
      JobId(job = parts(1).toInt, task = t.toInt, name = parts(0))
    }
  }

  /** Represents the identity of a job.
    *
    * @param job ID of the job
    * @param task ID of the task of array jobs
    * @param name the name of the job
    */
  case class JobId(job: Int, task: Int, name: String)

  object User {
    def apply(s: String): User = {
      val parts = s split ":"
      User(uid = parts(1), gid = parts(0))
    }
  }

  case class User(uid: String, gid: String)

  object Time {
    def seconds(s: String): Time = {
      val parts = s split ":" map { _.toLong * 1000L } map { _.toDateTime }
      Time(submission = parts(0), start = parts(1), end = parts(2))
    }

    def milliseconds(s: String): Time = {
      val parts = s split ":" map { _.toLong } map { _.toDateTime }
      Time(submission = parts(0), start = parts(1), end = parts(2))
    }
  }

  case class Time(submission: DateTime, start: DateTime, end: DateTime) {

    /** Returns the waiting time interval. */
    def waiting: Interval = submission to start

    /** Returns the running time interval. */
    def running: Interval = start to end

    /** Returns the turnaround time interval. */
    def turnaround: Interval = submission to end

  }

  object Status {
    def apply(s: String): Status = {
      val parts = s split ":" map { _.toInt }
      Status(grid = parts(0), script = parts(1))
    }
  }

  case class Status(grid: Int, script: Int) {
    def successful = grid == 0 && script == 0
    def failed = ! successful
  }

  object ResourceUsage {
    def apply(s: String, cpu: String, mem: String, maxvmem: String, io: String, iow: String): ResourceUsage = {
      val parts = s split ":"
      ResourceUsage(
        wctime    = parts(0).toDouble.round,
        utime     = parts(1).toDouble,
        stime     = parts(2).toDouble,
        maxrss    = parts(3).toDouble,
        ixrss     = parts(4).toLong,
//        ismrss    = parts(5).toLong,
        idrss     = parts(6).toLong,
        isrss     = parts(7).toLong,
        minflt    = parts(8).toLong,
        majflt    = parts(9).toLong,
        nswap     = parts(10).toLong,
        inblock   = parts(11).toDouble.round,
        oublock   = parts(12).toLong,
        msgsnd    = parts(13).toLong,
        msgrcv    = parts(14).toLong,
        nsignals  = parts(15).toLong,
        nvcsw     = parts(16).toLong,
        nivcsw    = parts(17).toLong,
        cputime   = cpu.toDouble,
        mem       = mem.toDouble,
        maxvmem   = maxvmem.toDouble.round,
        io        = io.toDouble,
        iow       = iow.toDouble
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
    * @param  cputime     cpu time usage in seconds
    * @param  mem         integral memory usage in Gbytes cpu seconds
    * @param  maxvmem     maximum vmem size in bytes
    * @param  io          amount of data transferred in input/output operations
    * @param  iow         io wait time in seconds
    */
  case class ResourceUsage (
      wctime: Long,
      utime: Double,
      stime: Double,
      maxrss: Double,
      ixrss: Long,
//      ismrss: Long,
      idrss: Long,
      isrss: Long,
      minflt: Long,
      majflt: Long,
      nswap: Long,
      inblock: Long,
      oublock: Long,
      msgsnd: Long,
      msgrcv: Long,
      nsignals: Long,
      nvcsw: Long,
      nivcsw: Long,
      cputime: Double,
      mem: Double,
      maxvmem: Long,
      io: Double,
      iow: Double
    ) {

    /** Returns the number of context switches, voluntary and involuntary. */
    def ncsw = nvcsw + nivcsw
  }

  object Acl {
    def apply(s: String): Acl = {
      val parts = s split ":"
      Acl(department = parts(1), project = parts(0))
    }
  }

  case class Acl(department: String, project: String)

  object Reservation {
    def apply(s: String): Option[Reservation] = {
      val parts = s split ":"

      val rid   = parts(0).toInt
      val rst   = parts(1).toLong

      (rid + rst) match {
        case 0 ⇒ None
        case _ ⇒ Some (
          Reservation(id = rid, submission = rst)
        )
      }
    }
  }

  case class Reservation(id: Int, submission: Long)

}

import Job._

case class Job (
    queue: Option[String],
    node: Option[String],
    user: User,
    id: JobId,
    account: String,
    priority: Double,
    time: Time,
    status: Status,
    res: ResourceUsage,
    acl: Acl,
    parallelEnvironment: Option[String],
    slots: Int,
    resReq: Option[String],
    parallelTaskId: Option[String],
    reservation: Option[Reservation]
  ) {

  /** Returns the efficiency.
    *
    * This value is calculated with the assumption that the job did not use more cores than
    * requested.
    */
  def efficiency: Double = (res.utime + res.stime) / slots / res.wctime

  def perMinute[A](f: Job => A): Map[DateTime,A] = {
    val start = time.start withSecondOfMinute 0
    val end   = time.end   withSecondOfMinute 0

    val value = f(this)

    start to end by 1.minute mapValue { value }
  }

  def waitPerMinute[A](f: Job => A): Map[DateTime,A] = {
    val submission = time.submission withSecondOfMinute 0
    val start      = time.start      withSecondOfMinute 0

    val value = f(this)

    submission to start by 1.minute mapValue { value }
  }

}
