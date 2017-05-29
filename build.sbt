enablePlugins(BuildInfoPlugin)
enablePlugins(GitVersioning)

name := "pretty-accounting"

scalaVersion := "2.12.2"

crossScalaVersions := Seq("2.11.11", "2.12.2")

buildInfoKeys := Seq[BuildInfoKey](name, version)

buildInfoPackage := "grid"

libraryDependencies ++= Seq (
  "com.itextpdf"             %  "itextpdf"      % "5.5.11",
  "com.github.wookietreiber" %% "scala-chart"   % "0.5.1",
  "com.github.nscala-time"   %% "nscala-time"   % "2.16.0",
  "co.fs2"                   %% "fs2-core"      % "0.9.6",
  "co.fs2"                   %% "fs2-io"        % "0.9.6",
  "co.fs2"                   %% "fs2-cats"      % "0.3.0",
  "com.github.scopt"         %% "scopt"         % "3.5.0",
  "org.specs2"               %% "specs2-core"   % "3.8.9" % "test"
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

fork in run := true
