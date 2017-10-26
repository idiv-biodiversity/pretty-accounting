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

// TODO merge exclude into filter
final case class Exclude(
  uids: List[String] = Nil,
  gids: List[String] = Nil
)

final case class Filter(
  project: Option[String] = None
)

final case class Config private (
  val accountingFiles: Seq[Path] = Nil,
  val start: Option[DateTime] = None,
  val end: Option[DateTime] = None,
  val filter: Filter = Filter(),
  val category: Option[Category] = None,
  val progress: Option[Int] = None,
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

    note("\nfilters:\n")

    opt[String]("filter-project")
      .valueName("project")
      .action((p, c) => c.copy(filter = c.filter.copy(project = Some(p))))
      .text("use only jobs from this project")

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

    // TODO iterations should be optional: [--progress [uint32]]
    opt[Int]("progress")
      .action((iterations, c) => c.copy(progress = Some(iterations)))
      .text("prints some status updates along the way")

    opt[Unit]('v', "verbose")
      .action((_, c) => c.copy(verbose = true))
      .text("prints what is going on under the hood")

    help("help").text("prints this usage text")

    version("version")

    note("")
  }
}
