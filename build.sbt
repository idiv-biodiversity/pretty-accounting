name := "pretty-accounting"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq (
  "com.itextpdf"             %  "itextpdf"      % "5.5.10",
  "com.github.wookietreiber" %% "scala-chart"   % "0.5.1",
  "com.github.nscala-time"   %% "nscala-time"   % "2.14.0",
  "co.fs2"                   %% "fs2-core"      % "0.9.2",
  "co.fs2"                   %% "fs2-io"        % "0.9.2",
  "co.fs2"                   %% "fs2-cats"      % "0.2.0",
  "org.specs2"               %% "specs2-core"   % "3.8.6" % "test"
)

initialCommands in Compile += """
  import cats._
  import cats.data._
  import cats.implicits._
  import fs2._
  import fs2.interop.cats._
  import scala.math._
  import com.github.nscala_time.time.Imports._
  import scalax.chart._
  import scalax.chart.api._
"""

initialCommands in (Compile, consoleQuick) := (initialCommands in Compile).value

initialCommands in Compile in console += """
  import grid._
  import grid.Accounting._
"""
