package grid

import fs2._
import java.nio.file._

import Filtering._

object Streaming extends Streaming

trait Streaming extends Parsing {

  // TODO throw out of this API and move to app config
  def accountingFile: String = sys.props get "grid.accounting.file" getOrElse {
    sys.env.getOrElse("SGE_ROOT", "/usr/local/sge") + "/default/common/accounting"
  }

  // TODO remove chunk size magic number / make customizable in app
  def lines(path: Path): Stream[Task,String] = {
    io.file.readAll[Task](path, chunkSize = math.pow(2,20).toInt)
      .through(text.utf8Decode)
      .through(text.lines)
  }

  def raw: Stream[Task,Job] =
    raw(Paths.get(accountingFile))

  def raw(path: Path): Stream[Task,Job] = {
    Parser(path) match {
      case Some(parser) =>
        // TODO if (verbose) println(s"[XXX] $path $parser")
        parser.parse(lines(path))

      case None =>
        // TODO print something about no parser being found
        Stream.empty
    }
  }

  def jobs(implicit conf: Config): Stream[Task,Job] = {
    raw filter combined
  }

  def dispatched(implicit conf: Config): Stream[Task,Job] =
    jobs filter isDispatched

}
