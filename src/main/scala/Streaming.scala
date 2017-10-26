package grid

import cats.implicits._
import fs2._
import java.nio.file._

import Filtering._

object Streaming extends Streaming

trait Streaming extends Parsing {

  // TODO remove chunk size magic number / make customizable in app
  def lines(path: Path)(implicit conf: Config): Stream[Task, String] = {
    val lines = io.file.readAll[Task](path, chunkSize = math.pow(2,20).toInt)
      .through(text.utf8Decode)
      .through(text.lines)

    conf.progress.fold(lines) { niterations =>
      lines.zipWithIndex map { case (line, index) =>
        if (index % niterations === 0) {
          Console.err.println(
            s"""[${thread.name}] [i=$index] $path"""
          )
        }

        line
      }
    }
  }

  def raw(path: Path)(implicit conf: Config): Stream[Task,Job] = {
    Parser(path) match {
      case Some(parser) =>
        if (conf.verbose) {
          Console.err.println(
            s"""[${thread.name}] using $parser for $path"""
          )
        }

        parser.parse(lines(path).filter(line => !line.startsWith("#") && line.trim.nonEmpty))

      case None =>
        Console.err.println(
          s"""[${thread.name}] no parser found for $path"""
        )
        Stream.empty
    }
  }

}
