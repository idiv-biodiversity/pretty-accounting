package grid

abstract class Job {

  // -----------------------------------------------------------------------------------------------
  // members - should be overridden with lazy vals
  // -----------------------------------------------------------------------------------------------

  def account: String
  def acl: Acl
  def id: JobId
  def node: Option[String]
  def parallelEnvironment: Option[String]
  def parallelTaskId: Option[String]
  def priority: Double
  def queue: Option[String]
  def res: ResourceUsage
  def resReq: Option[String]
  def reservation: Option[Reservation]
  def slots: Int
  def status: Status
  def time: Time
  def user: User

  // -----------------------------------------------------------------------------------------------
  // utility functions working with members
  // -----------------------------------------------------------------------------------------------

  final def efficiency: Double = (res.utime + res.stime) / slots / res.wctime

  final def perMinute[A](f: Job => A): Map[DateTime,A] = {
    val start = time.start withSecondOfMinute 0
    val end   = time.end   withSecondOfMinute 0

    val value = f(this)

    start to end by 1.minute mapValue { value }
  }

  final def waitPerMinute[A](f: Job => A): Map[DateTime,A] = {
    val submission = time.submission withSecondOfMinute 0
    val start      = time.start      withSecondOfMinute 0

    val value = f(this)

    submission to start by 1.minute mapValue { value }
  }

  // -----------------------------------------------------------------------------------------------
  // member classes
  // -----------------------------------------------------------------------------------------------

  abstract class Acl {
    def department: String
    def project: Option[String]
  }

  /** The identity of the job. */
  abstract class JobId {
    def job: Int
    def task: Int
    def name: String
  }

  abstract class Reservation {
    def id: Int
    def submission: Long
  }

  abstract class ResourceUsage {
    /** Returns the wallclock time in seconds. */
    def wctime: Double

    /** Returns the user time in seconds. */
    def utime: Double

    /** Returns the system time in seconds. */
    def stime: Double

    /** Returns the maximum resident set size. */
    def maxrss: Double = ???

    /** Returns the integral shared memory size. */
    def ixrss: Long = ???

    // TODO what is this and when is it used?
    // def ismrss: Long

    /** Returns the integral unshared data size. */
    def idrss: Long = ???

    /** Returns the integral unshared stack size. */
    def isrss: Long = ???

    /** Returns the number of page reclaims. */
    def minflt: Long = ???

    /** Returns the number of page faults. */
    def majflt: Long = ???

    /** Returns the number of swaps. */
    def nswap: Long = ???

    /** Returns the number of block input operations. */
    def inblock: Long = ???

    /** Returns the number of block output operations. */
    def oublock: Long = ???

    /** Returns the number of messages sent. */
    def msgsnd: Long = ???

    /** Returns the number of messages received. */
    def msgrcv: Long = ???

    /** Returns the number of signals received. */
    def nsignals: Long = ???

    /** Returns the number of voluntary context switches. */
    def nvcsw: Long = ???

    /** Returns the number of involuntary context switches. */
    def nivcsw: Long = ???

    /** Returns the time spent on cpu in seconds. */
    def cputime: Double

    /** Returns the integral memory usage in Gbytes cpu seconds. */
    def mem: Double

    /** Returns the maximum vmem size in bytes. */
    def maxvmem: Long

    /** Returns the amount of data transferred in input/output operations. */
    def io: Double

    /** Returns the io wait time in seconds. */
    def iow: Double

    /** Returns the number of context switches, both voluntary and involuntary. */
    final def ncsw: Long = nvcsw + nivcsw
  }

  abstract class Status {
    def grid: Int
    def script: Int

    final def successful = grid == 0 && script == 0
    final def failed = ! successful
  }

  object Time {
    def seconds(s: String): DateTime = {
      (s.toLong * 1000L).toDateTime
    }

    def milliseconds(s: String): DateTime = {
      s.toLong.toDateTime
    }
  }

  abstract class Time {
    def submission: DateTime
    def start: DateTime
    def end: DateTime

    final def waiting: Interval = submission to start
    final def running: Interval = start to end
    final def turnaround: Interval = submission to end
  }

  abstract class User {
    def uid: String
    def gid: String
  }

}
