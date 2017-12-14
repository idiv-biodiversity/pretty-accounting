package grid

abstract class Job {

  // --------------------------------------------------------------------------
  // members - should be overridden with lazy vals
  // --------------------------------------------------------------------------

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

  // --------------------------------------------------------------------------
  // utility functions working with members
  // --------------------------------------------------------------------------

  final def efficiency: Double = (res.utime + res.stime) / slots / res.wctime

  final def perMinute[A](f: Job => A): Map[DateTime,A] = {
    val s = time.start withSecondOfMinute 0 withMillisOfSecond 0
    val e = time.end   withSecondOfMinute 0 withMillisOfSecond 0

    val value = f(this)

    (s to e).byWith(1.minute)(value)
  }

  final def waitPerMinute[A](f: Job => A): Map[DateTime,A] = {
    val submission = time.submission withSecondOfMinute 0 withMillisOfSecond 0
    val start      = time.start      withSecondOfMinute 0 withMillisOfSecond 0

    val value = f(this)

    (submission to start).byWith(1.minute)(value)
  }

  // --------------------------------------------------------------------------
  // member classes
  // --------------------------------------------------------------------------

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

  abstract class Time {
    def submission: DateTime
    def start: DateTime
    def end: DateTime

    final def waiting: Either[String, Interval] = try {
      Right(submission to start)
    } catch {
      case e: IllegalArgumentException =>
        Left(s"""${e.getMessage} submission=${submission} start=${start}""")
    }

    final def running: Either[String, Interval] = try {
      Right(start to end)
    } catch {
      case e: IllegalArgumentException =>
        Left(s"""${e.getMessage} start=${start} end=${end}""")
    }

    final def turnaround: Either[String, Interval] = try {
      Right(submission to end)
    } catch {
      case e: IllegalArgumentException =>
        Left(s"""${e.getMessage} submission=${submission} end=${end}""")
    }
  }

  abstract class User {
    def uid: String
    def gid: String
  }

}

object Job {
  private val UNKNOWN = "UNKNOWN"
  private val NONE    = "NONE"

  trait Indexed {

    self: Job =>

    trait Base {
      def queue: Int
      def node: Int
      def gid: Int
      def uid: Int
      def name: Int
      def job: Int
      def acc: Int
      def prio: Int
      def tsub: Int
      def tstart: Int
      def tend: Int
      def stfail: Int
      def stexit: Int
      def ruwc: Int
      def ruutime: Int
      def rustime: Int
      def rumrss: Int
      def ruixrss: Int
      def ruismrs: Int
      def ruidrss: Int
      def ruisrss: Int
      def rumiflt: Int
      def rumaflt: Int
      def runswap: Int
      def ruinblk: Int
      def ruoublk: Int
      def rumsgsn: Int
      def rumsgrc: Int
      def runsig: Int
      def runvcsw: Int
      def runicsw: Int
      def aclproj: Int
      def acldep: Int
      def pe: Int
      def slots: Int
      def task: Int
      def cpu: Int
      def mem: Int
      def io: Int
      def req: Int
      def iow: Int
      def peid: Int
      def maxvmem: Int
    }

    type Index <: Base

    def index: Index

    protected def parts: Array[String]

    lazy val account = parts(index.acc)

    object acl extends Acl {
      lazy val department = parts(index.acldep)
      lazy val project = parts(index.aclproj) match {
        case NONE    => None
        case project => Some(project)
      }
    }

    object id extends JobId {
      lazy val name = parts(index.name)
      lazy val job  = parts(index.job).toInt
      lazy val task = parts(index.task).toInt
    }

    lazy val node = parts(index.node) match {
      case UNKNOWN => None
      case node    => Some(node)
    }

    lazy val parallelEnvironment = parts(index.pe) match {
      case NONE => None
      case pe   => Some(pe)
    }

    lazy val parallelTaskId = parts(index.peid) match {
      case NONE => None
      case s    => Some(s)
    }

    lazy val priority = parts(index.prio).toDouble

    lazy val queue = parts(index.queue) match {
      case UNKNOWN => None
      case queue => Some(queue)
    }

    lazy val resReq = parts(index.req) match {
      case NONE => None
      case s    => Some(s)
    }

    lazy val slots = parts(index.slots).toInt

    object status extends Status {
      lazy val grid = parts(index.stfail).toInt
      lazy val script = parts(index.stexit).toInt
    }

    object user extends User {
      lazy val uid = parts(index.uid)
      lazy val gid = parts(index.gid)
    }
  }

  object Indexed {
    object Memory {
      trait A {
        self: Job with Indexed =>
        object res extends ResourceUsage {
          lazy val wctime  = parts(index.ruwc).replaceAll(",", ".").toDouble
          lazy val utime   = parts(index.ruutime).replaceAll(",", ".").toDouble
          lazy val stime   = parts(index.rustime).replaceAll(",", ".").toDouble
          lazy val cputime = parts(index.cpu).replaceAll(",", ".").toDouble
          lazy val mem     = parts(index.mem).replaceAll(",", ".").toDouble
          lazy val io      = parts(index.io).replaceAll(",", ".").toDouble
          lazy val iow     = parts(index.iow).replaceAll(",", ".").toDouble
          lazy val maxvmem = (parts(index.maxvmem).replaceAll(",", ".").toDouble * 1000).round
        }
      }

      trait B {
        self: Job with Indexed =>
        object res extends ResourceUsage {
          lazy val wctime  = parts(index.ruwc).replaceAll(",", ".").toDouble
          lazy val utime   = parts(index.ruutime).replaceAll(",", ".").toDouble
          lazy val stime   = parts(index.rustime).replaceAll(",", ".").toDouble
          lazy val cputime = parts(index.cpu).replaceAll(",", ".").toDouble
          lazy val mem     = parts(index.mem).replaceAll(",", ".").toDouble
          lazy val io      = parts(index.io).replaceAll(",", ".").toDouble
          lazy val iow     = parts(index.iow).replaceAll(",", ".").toDouble
          lazy val maxvmem = parts(index.maxvmem).replaceAll(",", ".").toLong
        }
      }
    }

    object Reservation {
      trait N {
        self: Job with Indexed =>
        final override def reservation = None
      }

      trait Y {
        self: Job with Indexed =>

        trait ReservationIndex {
          self: Base =>
          def arid: Int
          def arsub: Int
        }

        type Index = Base with ReservationIndex

        lazy val reservation: Option[Reservation] = {
          val xid = parts(index.arid).toInt
          val s = parts(index.arsub).toLong

          if (xid > 0)
            Some(new Reservation {
              val id = xid
              val submission = s
            })
            else
              None
        }
      }
    }

    object Time {
      def seconds(s: String): DateTime = {
        (s.toLong * 1000L).toDateTime
      }

      def milliseconds(s: String): DateTime = {
        s.toLong.toDateTime
      }

      trait Seconds {
        self: Job with Indexed =>
        object time extends Time {
          lazy val submission = seconds(parts(index.tsub))
          lazy val start = seconds(parts(index.tstart))
          lazy val end = seconds(parts(index.tend))
        }
      }

      trait Millis {
        self: Job with Indexed =>
        object time extends Time {
          lazy val submission = milliseconds(parts(index.tsub))
          lazy val start = milliseconds(parts(index.tstart))
          lazy val end = milliseconds(parts(index.tend))
        }
      }
    }
  }
}
