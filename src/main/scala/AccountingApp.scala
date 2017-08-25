package grid

@deprecated("use new config", "")
trait AccountingApp extends App with Accounting {

  /** Returns the name of this app. */
  def name: String

  /** Returns the default output file extension. */
  def defaultExtension: String

  implicit val conf: Config = Config.empty // TODO remove after migration

  def filtered = conf.interval map { interval =>
    dispatched filter isBetween(interval)
  } getOrElse {
    dispatched
  }

  def extension: String = sys.props get "grid.accounting.output.extension" getOrElse {
    defaultExtension
  }

  def outputPath: String = sys.props get "grid.accounting.output.path" map { dir ⇒
    if (dir endsWith fileSeparator) dir substring (0, dir.length - 1) else dir
  } filter {
    _.isDirectory
  } getOrElse {
    sys.props get "java.io.tmpdir" getOrElse util.Properties.userHome
  }

  def output: String = "%s%s%s.%s" format (
    outputPath,
    fileSeparator,
    sys.props get "grid.accounting.output.name" getOrElse {
      name + ("-" + (conf.interval map { _.toString.replaceAll(fileSeparator,"-") } getOrElse { DateTime.now }))
    },
    extension
  )
}
