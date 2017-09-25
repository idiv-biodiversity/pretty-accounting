package grid

import java.nio.file.Path

import fs2._

object Parsing extends Parsing

trait Parsing {

  abstract class Parser(name: String) {
    final override def toString: String = name

    def parse(lines: Stream[Task, String]): Stream[Task, Job]
  }

  object Parser {

    /** Optionally returns the appropriate parser.
      *
      * Checks the first line of the given accounting file. If there is no
      * first line, `None` is returned since we don't need to parse anything
      * for this file.
      *
      * The first line, if it exists, is checked with this regex `"""# Version:
      * (.+)""".r`. If the version capture group starts with one of the
      * supported versions, the specific parser for that version is returned.
      */
    def apply(path: Path): Option[Parser] = {
      val first: Option[String] =
        io.file.readAll[Task](path, chunkSize = math.pow(2,20).toInt)
          .through(text.utf8Decode)
          .through(text.lines)
          .take(1)
          .runLog
          .unsafeRun()
          .headOption

      val V = """# Version: (.+)""".r

      first collect {
        case V(version) if version startsWith "6.0"  => Parser.sge60
        case V(version) if version startsWith "6.2"  => Parser.sge62
        case V(version) if version startsWith "8.3." => Parser.uge83
        case V(version) if version startsWith "8.4." => Parser.uge84
      }
    }

    val uge84 = new Parser("Univa Grid Engine 8.4") {
      final
      def parse(lines: Stream[Task, String]): Stream[Task, Job] =
        lines map { line => new J(line.split(":")) }

      final
      class J(protected val parts: Array[String])
          extends Job
          with Job.Indexed
          with Job.Indexed.Memory.B
          with Job.Indexed.Reservation.Y
          with Job.Indexed.Time.Millis
      {
        object index extends Base with ReservationIndex {
          val queue = 0
          val node = 1
          val gid = 2
          val uid = 3
          val name = 4
          val job = 5
          val acc = 6
          val prio = 7
          val tsub = 8
          val tstart = 9
          val tend = 10
          val stfail = 11
          val stexit = 12
          val ruwc = 13
          val ruutime = 14
          val rustime = 15
          val rumrss = 16
          val ruixrss = 17
          val ruismrs = 18
          val ruidrss = 19
          val ruisrss = 20
          val rumiflt = 21
          val rumaflt = 22
          val runswap = 23
          val ruinblk = 24
          val ruoublk = 25
          val rumsgsn = 26
          val rumsgrc = 27
          val runsig = 28
          val runvcsw = 29
          val runicsw = 30
          val aclproj = 31
          val acldep = 32
          val pe = 33
          val slots = 34
          val task = 35
          val cpu = 36
          val mem = 37
          val io = 38
          val req = 39
          val iow = 40
          val peid = 41
          val maxvmem = 42
          val marss = 43
          val mapss = 44
          val x = 45
          val delby = 46
          val arid = 47
          val arsub = 48
          val subno = 49
          val y = 50
          val subcm = 51
          val wallc = 52
          val z = 53
        }
      }
    }

    val uge83 = new Parser("Univa Grid Engine 8.3") {
      final
      def parse(lines: Stream[Task, String]): Stream[Task, Job] =
        lines map { line => new J(line.split(":")) }

      final
      class J(protected val parts: Array[String])
          extends Job
          with Job.Indexed
          with Job.Indexed.Memory.A
          with Job.Indexed.Reservation.Y
          with Job.Indexed.Time.Millis
      {
        object index extends Base with ReservationIndex {
          val queue = 0
          val node = 1
          val gid = 2
          val uid = 3
          val name = 4
          val job = 5
          val acc = 6
          val prio = 7
          val tsub = 8
          val tstart = 9
          val tend = 10
          val stfail = 11
          val stexit = 12
          val ruwc = 13
          val ruutime = 14
          val rustime = 15
          val rumrss = 16
          val ruixrss = 17
          val ruismrs = 18
          val ruidrss = 19
          val ruisrss = 20
          val rumiflt = 21
          val rumaflt = 22
          val runswap = 23
          val ruinblk = 24
          val ruoublk = 25
          val rumsgsn = 26
          val rumsgrc = 27
          val runsig = 28
          val runvcsw = 29
          val runicsw = 30
          val aclproj = 31
          val acldep = 32
          val pe = 33
          val slots = 34
          val task = 35
          val cpu = 36
          val mem = 37
          val io = 38
          val req = 39
          val iow = 40
          val peid = 41
          val maxvmem = 42
          val marss = 43
          val mapss = 44
          val x = 45
          val delby = 46
          val arid = 47
          val arsub = 48
          val subno = 49
          val y = 50
          val subcm = 51
          val wallc = 52
        }
      }
    }

    val sge62 = new Parser("Sun Grid Engine 6.2") {
      final
      def parse(lines: Stream[Task, String]): Stream[Task, Job] =
        lines map { line => new J(line.split(":")) }

      final
      class J(protected val parts: Array[String])
          extends Job
          with Job.Indexed
          with Job.Indexed.Memory.A
          with Job.Indexed.Reservation.Y
          with Job.Indexed.Time.Seconds
      {
        object index extends Base with ReservationIndex {
          val queue = 0
          val node = 1
          val gid = 2
          val uid = 3
          val name = 4
          val job = 5
          val acc = 6
          val prio = 7
          val tsub = 8
          val tstart = 9
          val tend = 10
          val stfail = 11
          val stexit = 12
          val ruwc = 13
          val ruutime = 14
          val rustime = 15
          val rumrss = 16
          val ruixrss = 17
          val ruismrs = 18
          val ruidrss = 19
          val ruisrss = 20
          val rumiflt = 21
          val rumaflt = 22
          val runswap = 23
          val ruinblk = 24
          val ruoublk = 25
          val rumsgsn = 26
          val rumsgrc = 27
          val runsig = 28
          val runvcsw = 29
          val runicsw = 30
          val aclproj = 31
          val acldep = 32
          val pe = 33
          val slots = 34
          val task = 35
          val cpu = 36
          val mem = 37
          val io = 38
          val req = 39
          val iow = 40
          val peid = 41
          val maxvmem = 42
          val arid = 43
          val arsub = 44
        }
      }
    }

    val sge60 = new Parser("Sun Grid Engine 6.0") {
      final
      def parse(lines: Stream[Task, String]): Stream[Task, Job] =
        lines map { line => new J(line.split(":")) }

      final
      class J(protected val parts: Array[String])
          extends Job
          with Job.Indexed
          with Job.Indexed.Memory.A
          with Job.Indexed.Reservation.N
          with Job.Indexed.Time.Seconds
      {
        type Index = Base
        object index extends Index {
          val queue = 0
          val node = 1
          val gid = 2
          val uid = 3
          val name = 4
          val job = 5
          val acc = 6
          val prio = 7
          val tsub = 8
          val tstart = 9
          val tend = 10
          val stfail = 11
          val stexit = 12
          val ruwc = 13
          val ruutime = 14
          val rustime = 15
          val rumrss = 16
          val ruixrss = 17
          val ruismrs = 18
          val ruidrss = 19
          val ruisrss = 20
          val rumiflt = 21
          val rumaflt = 22
          val runswap = 23
          val ruinblk = 24
          val ruoublk = 25
          val rumsgsn = 26
          val rumsgrc = 27
          val runsig = 28
          val runvcsw = 29
          val runicsw = 30
          val aclproj = 31
          val acldep = 32
          val pe = 33
          val slots = 34
          val task = 35
          val cpu = 36
          val mem = 37
          val io = 38
          val req = 39
          val iow = 40
          val peid = 41
          val maxvmem = 42
        }
      }
    }

  }

}
