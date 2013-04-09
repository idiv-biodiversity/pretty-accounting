
name := "pretty-accounting"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq (
  "com.github.wookietreiber"      %% "scala-chart"   % "0.2.0",
  "com.github.nscala-time"        %% "nscala-time"   % "0.4.0",
  "org.scalaz"                    %% "scalaz-core"   % "6.0.4",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2",
  "org.specs2"                    %% "specs2"        % "1.14" % "test"
)

initialCommands in Compile += """
  import scalaz._
  import Scalaz._
  import scala.math._
  import com.github.nscala_time.time.Imports._
  import scalax.chart._
  import scalax.chart.Charting._
"""

initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile

initialCommands in Compile in console += """
  import grid._
  import grid.Accounting._
"""

