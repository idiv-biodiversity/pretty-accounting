name := "pretty-accounting"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq (
  "com.itextpdf"             %  "itextpdf"      % "5.5.8",
  "com.github.wookietreiber" %% "scala-chart"   % "0.5.0",
  "com.github.nscala-time"   %% "nscala-time"   % "2.6.0",
  "org.scalaz.stream"        %% "scalaz-stream" % "0.8",
  "org.specs2"               %% "specs2-core"   % "3.7" % "test"
)

initialCommands in Compile += """
  import scalaz._
  import Scalaz._
  import scala.math._
  import com.github.nscala_time.time.Imports._
  import scalax.chart._
  import scalax.chart.module.Charting._
"""

initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile

initialCommands in Compile in console += """
  import grid._
  import grid.Accounting._
"""
