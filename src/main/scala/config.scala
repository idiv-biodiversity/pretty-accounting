package grid

import java.nio.file.{Path, Paths}

case class Config(accountingFiles: Seq[Path] = Nil,
                  start: Option[LocalDate] = None,
                  end: Option[LocalDate] = None,
                  threads: Int = 1) {

  /** Returns true if the job has been started between the start and end dates.
    *
    * If start is not given, the beginning of the universe is assumed. If end is not given, the end
    * of the universe is assumed.
    */
  def startedBetween(job: Job): Boolean =
    start.fold(true)(job.time.start.toLocalDate >= _) &&
      end.fold(true)(job.time.start.toLocalDate < _)

}

object Config {
  implicit val pathRead: scopt.Read[Path] =
    scopt.Read.reads(Paths.get(_))

  implicit val dateRead: scopt.Read[LocalDate] =
    scopt.Read.reads(_.toLocalDate)

  def parser(name: String) = new scopt.OptionParser[Config](name) {
    head(name, BuildInfo.version)

    opt[Seq[Path]]('f', "file")
      .required()
      .valueName("file1,file2...")
      .action((x, c) => c.copy(accountingFiles = x))
      .text("accounting files to include")

    opt[LocalDate]('s', "start")
      .valueName("2016-01-01")
      .action((x, c) => c.copy(start = Some(x)))
      .text("jobs started at this date")

    opt[LocalDate]('e', "end")
      .valueName("2017-01-01")
      .action((x, c) => c.copy(end = Some(x)))
      .text("jobs started before this date")

    opt[Int]("threads")
      .valueName(sys.runtime.availableProcessors.toString)
      .action((x, c) => c.copy(threads = x))
      .text("use at most CPUs for concurrent/parallel processing")

    help("help").text("prints this usage text")
  }
}
