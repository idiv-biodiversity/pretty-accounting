package grid

import language.higherKinds

import cats.implicits._
import enumeratum._
import fs2._
import java.nio.file._

sealed abstract class Category extends EnumEntry
object Category extends Enum[Category] {
  val values = findValues

  def isDefinedAt(name: String): Boolean =
    lowerCaseNamesToValuesMap isDefinedAt name

  final case object Department extends Category
  final case object Project extends Category
}

sealed abstract class Output extends EnumEntry
object Output extends Enum[Output] {
  val values = findValues

  def isDefinedAt(name: String): Boolean =
    lowerCaseNamesToValuesMap isDefinedAt name

  final case object Chart extends Output
  final case object CSV extends Output
  final case object Table extends Output
}

final case class Exclude(
  uids: List[String] = Nil,
  gids: List[String] = Nil
)

final case class Config private (
  val accountingFiles: Seq[Path] = Nil,
  val start: Option[DateTime] = None,
  val end: Option[DateTime] = None,
  val category: Option[Category] = None,
  val progress: Boolean = false,
  val resolution: (Int, Int) = (1920, 1080),
  val output: Output = Output.Table,
  val verbose: Boolean = false,
  val threads: Int,
  val exclude: Exclude,
  val association: Map[String, Map[String, String]]
) {

  /** Optionally returns the interval from start to end. */
  def interval: Option[Interval] =
    (start, end).mapN(_ to _)

  /** Maps a stream and optionally prints progress.
    *
    * Progress is printed every 10000 items. If the `progress` flag is not set,
    * `s.map(f)` is all that will be done.
    */
  def mapWithProgress[F[_], A, B](s: Stream[F, A], file: Path)(f: A => B): Stream[F, B] = {
    if (!progress) {
      // simple case, no progress
      s.map(f)
    } else {
      // TODO make niterations costumizable: [--progress [uint32]], scopt can't
      val niterations = 10000

      // TODO better way to do this? maybe some fs2 way
      // display index every once in a while
      s.zipWithIndex map { case (a, index) =>
        if (index % niterations === 0) {
          Console.err.println(
            s"""[${Thread.currentThread.getName}] [i=$index] $file"""
          )
        }

        f(a)
      }
    }
  }

  /** Returns true if the job has been started between the start and end dates.
    *
    * If start is not given, the beginning of the universe is assumed. If end
    * is not given, the end of the universe is assumed.
    */
  def startedBetween(job: Job): Boolean =
    start.fold(true)(job.time.start >= _) &&
      end.fold(true)(job.time.start < _)

  /** Returns the amount of threads and the strategy for parallel execution.
    *
    * The amount of threads used will be downgraded if there are fewer files
    * than threads specified via the `--threads` option. If this happens and
    * the `--verbose` flag is set, there will be a note about it on STDERR.
    *
    * {{{
    * val (threads, strategy) = conf.strategy
    * implicit val S = strategy
    * // ...
    * val stream = fs2.concurrent.join(maxOpen = threads)(streams)
    * }}}
    */
  def strategy: (Int, Strategy) = {
    // no more than `files.size` threads needed
    val t = threads min accountingFiles.size

    if (verbose && t < threads) {
      Console.err.println(
        // TODO localize
        s"""there are only $t files, downgrading $threads to $t threads"""
      )
    }

    t -> fs2.Strategy.fromFixedDaemonPool(t)
  }
}

object Config {

  final case class FromFile(
    threads: Int,
    exclude: Exclude,
    association: Map[String, Map[String, String]]
  )

  private val fromFile: Option[FromFile] = {
    val p = Paths.get(scala.util.Properties.userHome, ".pa.conf")
    pureconfig.loadConfig[FromFile](p).toOption
  }

  def empty: Config = {
    Config(
      threads     = fromFile.map(_.threads    ).getOrElse(1),
      exclude     = fromFile.map(_.exclude    ).getOrElse(Exclude()),
      association = fromFile.map(_.association).getOrElse(Map())
    )
  }

  implicit val pathRead: scopt.Read[Path] =
    scopt.Read.reads(Paths.get(_))

  implicit val dateRead: scopt.Read[DateTime] =
    scopt.Read.reads(_.toDateTime)

  private val resolutionRegex = """(\d+)x(\d+)""".r

  def parser(name: String) = new scopt.OptionParser[Config](name) {
    head(name, BuildInfo.version)

    arg[Path]("file...")
      .unbounded()
      .action((x, c) => c.copy(accountingFiles = c.accountingFiles :+ x))
      .text("accounting files to include")

    note("\noptions:\n")

    opt[DateTime]('s', "start")
      .valueName("2016-01-01")
      .action((x, c) => c.copy(start = Some(x)))
      .text("jobs started after this date")

    opt[DateTime]('e', "end")
      .valueName("2017-01-01")
      .action((x, c) => c.copy(end = Some(x)))
      .text("jobs started before this date")

    opt[String]('g', "groupby")
      .valueName("category")
      .validate(x =>
        if (Category.lowerCaseNamesToValuesMap.isDefinedAt(x)) { success }
        else { failure(s"unknown category: $x") }
      )
      .action((x, c) => c.copy(category = Category.withNameLowercaseOnlyOption(x)))
      .text(s"""group by category, categories: ${Category.lowerCaseNamesToValuesMap.keys.mkString(", ")}""")

    note("\nother options:\n")

    opt[String]("output-format")
      .valueName("output")
      .validate(x =>
        if (Output.lowerCaseNamesToValuesMap.isDefinedAt(x)) { success }
        else { failure(s"unknown output: $x") }
      )
      .action((x, c) => c.copy(output = Output.withNameLowercaseOnly(x)))
      .text(s"""output format, formats: ${Output.lowerCaseNamesToValuesMap.keys.mkString(", ")}""")

    opt[String]("resolution")
      .valueName("1920x1080")
      .validate(x =>
        if (resolutionRegex.unapplySeq(x).isDefined) { success }
        else { failure(s"invalid resolution: $x") }
      )
      .action((x, c) =>
        x match {
          case resolutionRegex(width, height) =>
            c.copy(resolution = (width.toInt, height.toInt))
        }
      )
      .text("use this output resolution for charts")

    opt[Int]("threads")
      .valueName(sys.runtime.availableProcessors.toString)
      .action((x, c) => c.copy(threads = x))
      .text("use at most CPUs for concurrent/parallel processing")

    opt[Unit]("progress")
      .action((_, c) => c.copy(progress = true))
      .text("prints some status updates along the way")

    opt[Unit]('v', "verbose")
      .action((_, c) => c.copy(verbose = true))
      .text("prints what is going on under the hood")

    help("help").text("prints this usage text")

    version("version")

    note("")
  }
}
