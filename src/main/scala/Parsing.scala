package grid

import java.nio.file.Path

import fs2._

object Parsing extends Parsing

trait Parsing {

  // TODO make job parsing lazy because usually only a few columns are needed

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

      //             queue   node    u(gid  uid  ) j(name id   ) acc     prio    t(sub start end   ) st(fail exit) ru(wc  utime stime mrss  ixrss ismrs idrss isrss miflt maflt nswap inblk oublk msgsn msgrc nsig  nvcsw nicsw) acl(proj dep) pe      slots   task    cpu     mem     io      req  iow     pe_id   maxvmem marss mapss NONE? delby ar(id    sub) subno NONE? subcm wallc ioops?
      val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):[^:]+:[^:]+:[^:]+:[^:]+:([^:]+:[^:]+):[^:]+:[^:]+:[^:]+:[^:]+:[^:]+""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines collect {
          case regex(queue, node, user, jobId, account, priority, time, status, res, acl,
            parallelEnvironment, slots, taskNumber, cpu, mem, io, resReq, iow, parallelTaskId,
            maxvmem, reservation) =>

            import Job._

            Job(
              queue match { case UNKNOWN => None ; case queue => Some(queue) },
              node  match { case UNKNOWN => None ; case node  => Some(node)  },
              User(user),
              JobId(jobId, taskNumber),
              account,
              priority.toDouble,
              Time.milliseconds(time),
              Status(status),
              ResourceUsage(res,cpu,mem,maxvmem,io,iow),
              Acl(acl),
              parallelEnvironment match { case NONE => None ; case s => Some(s) },
              slots.toInt,
              resReq         match { case NONE => None ; case s => Some(s) },
              parallelTaskId match { case NONE => None ; case s => Some(s) },
              Reservation(reservation)
            )
        }
    }

    val uge83 = new Parser {
      override def toString: String = "uge83"

      //             queue   node    u(gid  uid  ) j(name id   ) acc     prio    t(sub start end   ) st(fail exit) ru(wc  utime stime mrss  ixrss ismrs idrss isrss miflt maflt nswap inblk oublk msgsn msgrc nsig  nvcsw nicsw) acl(proj dep) pe      slots   task    cpu     mem     io      req  iow     pe_id   maxvmem marss mapss NONE? delby ar(id    sub) subno NONE? subcm wallc
      val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):[^:]+:[^:]+:[^:]+:[^:]+:([^:]+:[^:]+):[^:]+:[^:]+:[^:]+:[^:]+""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines collect {
          case regex(queue, node, user, jobId, account, priority, time, status, res, acl,
            parallelEnvironment, slots, taskNumber, cpu, mem, io, resReq, iow, parallelTaskId,
            maxvmem, reservation) =>

            import Job._

            Job(
              queue match { case UNKNOWN => None ; case queue => Some(queue) },
              node  match { case UNKNOWN => None ; case node  => Some(node)  },
              User(user),
              JobId(jobId, taskNumber),
              account,
              priority.toDouble,
              Time.milliseconds(time),
              Status(status),
              ResourceUsage(res,cpu,mem,maxvmem,io,iow),
              Acl(acl),
              parallelEnvironment match { case NONE => None ; case s => Some(s) },
              slots.toInt,
              resReq         match { case NONE => None ; case s => Some(s) },
              parallelTaskId match { case NONE => None ; case s => Some(s) },
              Reservation(reservation)
            )
        }
    }

    val sge62 = new Parser {
      override def toString: String = "sge62"

      //             queue   host   u(gid   uid  )j(name  id   ) acc     prio   t(sub   start end  ) st(fail exit) ru(wc  utime stime maxr  ixr   ismr  idr   isr   minfl majfl nswap inblk oublk msgsn msgrc signs nvcsw nvcsw)a(proj  dep  ) pe      slots   taskid  cpu     mem     io      req  iow     pe_id   maxvmem ar(id  sub)
      val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):([^:]+:[^:]+)""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines collect {
          case regex(queue, node, user, jobId, account, priority, time, status, res, acl,
            parallelEnvironment, slots, taskNumber, cpu, mem, io, resReq, iow, parallelTaskId,
            maxvmem, reservation) =>

            import Job._

            Job(
              queue match { case UNKNOWN => None ; case queue => Some(queue) },
              node  match { case UNKNOWN => None ; case node  => Some(node)  },
              User(user),
              JobId(jobId, taskNumber),
              account,
              priority.toDouble,
              Time.seconds(time),
              Status(status),
              ResourceUsage(res,cpu,mem,maxvmem,io,iow),
              Acl(acl),
              parallelEnvironment match { case NONE => None ; case s => Some(s) },
              slots.toInt,
              resReq         match { case NONE => None ; case s => Some(s) },
              parallelTaskId match { case NONE => None ; case s => Some(s) },
              Reservation(reservation)
            )
        }
    }

    val sge60 = new Parser {
      override def toString: String = "sge60"

      val regex = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+)""".r

      def parse(lines: Stream[Task,String]): Stream[Task,Job] =
        lines collect {
          case regex(queue, node, user, jobId, account, priority, time, status, res, acl,
            parallelEnvironment, slots, taskNumber, cpu, mem, io, resReq, iow, parallelTaskId,
            maxvmem) =>

            import Job._

            Job(
              queue match { case UNKNOWN => None ; case queue => Some(queue) },
              node  match { case UNKNOWN => None ; case node  => Some(node)  },
              User(user),
              JobId(jobId, taskNumber),
              account,
              priority.toDouble,
              Time.seconds(time),
              Status(status),
              ResourceUsage(res,cpu,mem,maxvmem,io,iow),
              Acl(acl),
              parallelEnvironment match { case NONE => None ; case s => Some(s) },
              slots.toInt,
              resReq         match { case NONE => None ; case s => Some(s) },
              parallelTaskId match { case NONE => None ; case s => Some(s) },
              None
            )
        }
    }

  }

  private val UNKNOWN = "UNKNOWN"
  private val NONE    = "NONE"

}
