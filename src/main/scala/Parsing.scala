package grid

import java.nio.file.Path

import fs2._

object Parsing extends Parsing

trait Parsing {

  trait Parser {
    def parse(lines: Stream[Task,String]): Stream[Task,Job]
  }

  object Parser {

    /** Optionally returns an appropriate parser.
      *
      * Checks the first line of the given accounting file. If there is no first line, `None` is
      * returned since we don't need to parse anything for this file.
      *
      * The first line, if it exists, is checked with this regex `"""# Version: (.+)""".r`. If the
      * version capture group starts with one of the supported versions, the specific parser for
      * that version is returned.
      */
    def apply(path: Path): Option[Parser] = {
      val first: Option[String] = io.file.readAll[Task](path, chunkSize = math.pow(2,20).toInt)
        .through(text.utf8Decode)
        .through(text.lines)
        .take(1).runLog.unsafeRun.headOption

      val Version = """# Version: (.+)""".r

      first collect {
        case Version(version) if version startsWith "6.0"  => Parser.sge60
        case Version(version) if version startsWith "6.2"  => Parser.sge62
        case Version(version) if version startsWith "8.3." => Parser.uge83
        case Version(version) if version startsWith "8.4." => Parser.uge84
      }
    }

    val uge84 = new Parser {
      override def toString: String = "uge84"

      //                0       1         2    3        4    5      6       7         8   9     10         11   12       13  14    15    16    17    18    19    20    21    22    23    24    25    26    27    28    29    30         31   32   33      34      35      36      37      38      39   40      41      42      43    44    45    46       47    48   49    50    51    52    53
      //                queue   node    u(gid  uid  ) j(name id   ) acc     prio    t(sub start end   ) st(fail exit) ru(wc  utime stime mrss  ixrss ismrs idrss isrss miflt maflt nswap inblk oublk msgsn msgrc nsig  nvcsw nicsw) acl(proj dep) pe      slots   task    cpu     mem     io      req  iow     pe_id   maxvmem marss mapss NONE? delby ar(id    sub) subno NONE? subcm wallc ioops?
      // val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):[^:]+:[^:]+:[^:]+:[^:]+:([^:]+:[^:]+):[^:]+:[^:]+:[^:]+:[^:]+:[^:]+""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines map { line =>
          new Job {
            private val parts = line.split(":")

            lazy val account = parts(6)

            object acl extends Acl {
              lazy val department = parts(32)
              lazy val project = parts(31) match {
                case NONE    => None
                case project => Some(project)
              }
            }

            object id extends JobId {
              lazy val name = parts(4)
              lazy val job  = parts(5).toInt
              lazy val task = parts(35).toInt
            }

            lazy val node = parts(1) match {
              case UNKNOWN => None
              case node    => Some(node)
            }

            lazy val parallelEnvironment = parts(33) match {
              case NONE => None
              case pe   => Some(pe)
            }

            lazy val parallelTaskId = parts(41) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val priority = parts(7).toDouble

            lazy val queue = parts(0) match {
              case UNKNOWN => None
              case queue   => Some(queue)
            }

            lazy val reservation: Option[Reservation] = {
              val i = parts(47).toInt
              val s = parts(48).toLong

              if (i > 0)
                Some(new Reservation {
                  val id = i
                  val submission = s
                })
              else
                None
            }

            object res extends ResourceUsage {
              lazy val wctime  = parts(13).replaceAll(",", ".").toDouble
              lazy val utime   = parts(14).replaceAll(",", ".").toDouble
              lazy val stime   = parts(15).replaceAll(",", ".").toDouble
              lazy val cputime = parts(36).replaceAll(",", ".").toDouble
            }

            lazy val resReq = parts(39) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val slots = parts(34).toInt

            object status extends Status {
              lazy val grid = parts(11).toInt
              lazy val script = parts(12).toInt
            }

            object time extends Time {
              lazy val submission = Time.milliseconds(parts(8))
              lazy val start = Time.milliseconds(parts(9))
              lazy val end = Time.milliseconds(parts(10))
            }

            object user extends User {
              lazy val uid = parts(3)
              lazy val gid = parts(2)
            }
          }
        }
    }

    val uge83 = new Parser {
      override def toString: String = "uge83"

      //                0       1         2    3        4    5      6       7         8   9     10         11   12       13  14    15    16    17    18    19    20    21    22    23    24    25    26    27    28    29    30         31   32   33      34      35      36      37      38      39   40      41      42      43    44    45    46       47    48   49    50    51    52
      //                queue   node    u(gid  uid  ) j(name id   ) acc     prio    t(sub start end   ) st(fail exit) ru(wc  utime stime mrss  ixrss ismrs idrss isrss miflt maflt nswap inblk oublk msgsn msgrc nsig  nvcsw nicsw) acl(proj dep) pe      slots   task    cpu     mem     io      req  iow     pe_id   maxvmem marss mapss NONE? delby ar(id    sub) subno NONE? subcm wallc
      // val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):[^:]+:[^:]+:[^:]+:[^:]+:([^:]+:[^:]+):[^:]+:[^:]+:[^:]+:[^:]+""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines map { line =>
          new Job {
            private val parts = line.split(":")

            lazy val account = parts(6)

            object acl extends Acl {
              lazy val department = parts(32)
              lazy val project = parts(31) match {
                case NONE    => None
                case project => Some(project)
              }
            }

            object id extends JobId {
              lazy val name = parts(4)
              lazy val job  = parts(5).toInt
              lazy val task = parts(35).toInt
            }

            lazy val node = parts(1) match {
              case UNKNOWN => None
              case node    => Some(node)
            }

            lazy val parallelEnvironment = parts(33) match {
              case NONE => None
              case pe   => Some(pe)
            }

            lazy val parallelTaskId = parts(41) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val priority = parts(7).toDouble

            lazy val queue = parts(0) match {
              case UNKNOWN => None
              case queue => Some(queue)
            }

            lazy val reservation: Option[Reservation] = {
              val i = parts(47).toInt
              val s = parts(48).toLong

              if (i > 0)
                Some(new Reservation {
                  val id = i
                  val submission = s
                })
              else
                None
            }

            object res extends ResourceUsage {
              lazy val wctime  = parts(13).replaceAll(",", ".").toDouble
              lazy val utime   = parts(14).replaceAll(",", ".").toDouble
              lazy val stime   = parts(15).replaceAll(",", ".").toDouble
              lazy val cputime = parts(36).replaceAll(",", ".").toDouble
            }

            lazy val resReq = parts(39) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val slots = parts(34).toInt

            object status extends Status {
              lazy val grid = parts(11).toInt
              lazy val script = parts(12).toInt
            }

            object time extends Time {
              lazy val submission = Time.milliseconds(parts(8))
              lazy val start = Time.milliseconds(parts(9))
              lazy val end = Time.milliseconds(parts(10))
            }

            object user extends User {
              lazy val uid = parts(3)
              lazy val gid = parts(2)
            }
          }
        }
    }

    val sge62 = new Parser {
      override def toString: String = "sge62"

      //                0       1         2    3       4     5      6       7         8   9     10         11   12       13  14    15    16    17    18    19    20    21    22    23    24    25    26    27    28    29    30         31   32   33      34      35      36      37      38      39   40      41      42         43  44
      //                queue   host   u(gid   uid  )j(name  id   ) acc     prio   t(sub   start end  ) st(fail exit) ru(wc  utime stime maxr  ixr   ismr  idr   isr   minfl majfl nswap inblk oublk msgsn msgrc signs nvcsw nvcsw)a(proj  dep  ) pe      slots   taskid  cpu     mem     io      req  iow     pe_id   maxvmem ar(id  sub)
      // val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):([^:]+:[^:]+)""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines map { line =>
          new Job {
            private val parts = line.split(":")

            lazy val account = parts(6)

            object acl extends Acl {
              lazy val department = parts(32)
              lazy val project = parts(31) match {
                case NONE    => None
                case project => Some(project)
              }
            }

            object id extends JobId {
              lazy val name = parts(4)
              lazy val job  = parts(5).toInt
              lazy val task = parts(35).toInt
            }

            lazy val node = parts(1) match {
              case UNKNOWN => None
              case node    => Some(node)
            }

            lazy val parallelEnvironment = parts(33) match {
              case NONE => None
              case pe   => Some(pe)
            }

            lazy val parallelTaskId = parts(41) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val priority = parts(7).toDouble

            lazy val queue = parts(0) match {
              case UNKNOWN => None
              case queue => Some(queue)
            }

            lazy val reservation: Option[Reservation] = {
              val i = parts(43).toInt
              val s = parts(44).toLong

              if (i > 0)
                Some(new Reservation {
                  val id = i
                  val submission = s
                })
              else
                None
            }

            object res extends ResourceUsage {
              lazy val wctime  = parts(13).replaceAll(",", ".").toDouble
              lazy val utime   = parts(14).replaceAll(",", ".").toDouble
              lazy val stime   = parts(15).replaceAll(",", ".").toDouble
              lazy val cputime = parts(36).replaceAll(",", ".").toDouble
            }

            lazy val resReq = parts(39) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val slots = parts(34).toInt

            object status extends Status {
              lazy val grid = parts(11).toInt
              lazy val script = parts(12).toInt
            }

            object time extends Time {
              lazy val submission = Time.milliseconds(parts(8))
              lazy val start = Time.milliseconds(parts(9))
              lazy val end = Time.milliseconds(parts(10))
            }

            object user extends User {
              lazy val uid = parts(3)
              lazy val gid = parts(2)
            }
          }
        }
    }

    val sge60 = new Parser {
      override def toString: String = "sge60"

      //                0       1         2    3        4    5      6       7         8   9     10         11   12       13  14    15    16    17    18    19    20    21    22    23    24    25    26    27    28    29    30         31   32   33      34      35      36      37      38      39   40      41      42
      //                queue   node    u(gid  uid  ) j(name id   ) acc     prio    t(sub start end   ) st(fail exit) ru(wc  utime stime mrss  ixrss ismrs idrss isrss miflt maflt nswap inblk oublk msgsn msgrc nsig  nvcsw nicsw) acl(proj dep) pe      slots   task    cpu     mem     io      req  iow     pe_id   maxvmem
      // val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+)""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines map { line =>
          new Job {
            private val parts = line.split(":")

            lazy val account = parts(6)

            object acl extends Acl {
              lazy val department = parts(32)
              lazy val project = parts(31) match {
                case NONE    => None
                case project => Some(project)
              }
            }

            object id extends JobId {
              lazy val name = parts(4)
              lazy val job  = parts(5).toInt
              lazy val task = parts(35).toInt
            }

            lazy val node = parts(1) match {
              case UNKNOWN => None
              case node    => Some(node)
            }

            lazy val parallelEnvironment = parts(33) match {
              case NONE => None
              case pe   => Some(pe)
            }

            lazy val parallelTaskId = parts(41) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val priority = parts(7).toDouble

            lazy val queue = parts(0) match {
              case UNKNOWN => None
              case queue => Some(queue)
            }

            lazy val reservation = None

            object res extends ResourceUsage {
              lazy val wctime  = parts(13).replaceAll(",", ".").toDouble
              lazy val utime   = parts(14).replaceAll(",", ".").toDouble
              lazy val stime   = parts(15).replaceAll(",", ".").toDouble
              lazy val cputime = parts(36).replaceAll(",", ".").toDouble
            }

            lazy val resReq = parts(39) match {
              case NONE => None
              case s    => Some(s)
            }

            lazy val slots = parts(34).toInt

            object status extends Status {
              lazy val grid = parts(11).toInt
              lazy val script = parts(12).toInt
            }

            object time extends Time {
              lazy val submission = Time.milliseconds(parts(8))
              lazy val start = Time.milliseconds(parts(9))
              lazy val end = Time.milliseconds(parts(10))
            }

            object user extends User {
              lazy val uid = parts(3)
              lazy val gid = parts(2)
            }
          }
        }
    }

  }

  private val UNKNOWN = "UNKNOWN"
  private val NONE    = "NONE"

}
