
name := "pretty-accounting"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq (
  "com.lowagie"                   %  "itext"         % "4.2.1",
  "com.github.wookietreiber"      %% "scala-chart"   % "0.4.2",
  "com.github.nscala-time"        %% "nscala-time"   % "1.0.0",
  "org.scalaz"                    %% "scalaz-core"   % "7.0.6",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3",
  "org.specs2"                    %% "specs2-core"   % "2.3.11" % "test"
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

