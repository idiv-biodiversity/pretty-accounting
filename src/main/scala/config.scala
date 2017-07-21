package grid

import java.nio.file.{Path, Paths}

import cats.implicits._

case class Config(accountingFiles: Seq[Path] = Nil,
                  start: Option[DateTime] = None,
                  end: Option[DateTime] = None,
                  progress: Boolean = false,
                  verbose: Boolean = false,
                  threads: Int = 1) {

  /** Optionally returns the interval from start to end. */
  def interval: Option[Interval] =
    (start, end).mapN(_ to _)

  /** Returns true if the job has been started between the start and end dates.
    *
    * If start is not given, the beginning of the universe is assumed. If end is not given, the end
    * of the universe is assumed.
    */
  def startedBetween(job: Job): Boolean =
    start.fold(true)(job.time.start >= _) &&
      end.fold(true)(job.time.start < _)

}

object Config {
  implicit val pathRead: scopt.Read[Path] =
    scopt.Read.reads(Paths.get(_))

  implicit val dateRead: scopt.Read[DateTime] =
    scopt.Read.reads(_.toDateTime)

  def parser(name: String) = new scopt.OptionParser[Config](name) {
    head(name, BuildInfo.version)

    opt[Seq[Path]]('f', "file")
      .required()
      .valueName("file1,file2...")
      .action((x, c) => c.copy(accountingFiles = x))
      .text("accounting files to include")

    opt[DateTime]('s', "start")
      .valueName("2016-01-01")
      .action((x, c) => c.copy(start = Some(x)))
      .text("jobs started at this date")

    opt[DateTime]('e', "end")
      .valueName("2017-01-01")
      .action((x, c) => c.copy(end = Some(x)))
      .text("jobs started before this date")

    opt[Unit]("progress")
      .action((_, c) => c.copy(progress = true))
      .text("prints some status updates along the way")

    opt[Unit]('v', "verbose")
      .action((_, c) => c.copy(verbose = true))
      .text("prints what is going on under the hood")

    opt[Int]("threads")
      .valueName(sys.runtime.availableProcessors.toString)
      .action((x, c) => c.copy(threads = x))
      .text("use at most CPUs for concurrent/parallel processing")

    help("help").text("prints this usage text")
  }
}
