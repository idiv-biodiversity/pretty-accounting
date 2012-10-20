
name := "pretty-accounting"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0-RC1"

libraryDependencies ++= Seq (
  "org.sfree"                     %% "sfreechart"            % "latest.integration",
  "org.scala-tools.time"          %  "time_2.9.1"            % "0.5",
  "org.scalaz"                    %  "scalaz-core_2.10.0-M7" % "6.0.4",
  "com.github.scala-incubator.io" %% "scala-io-core"         % "0.4.1",
  "com.github.scala-incubator.io" %% "scala-io-file"         % "0.4.1",
  "org.specs2"                    %% "specs2"                % "1.12.2" % "test" cross CrossVersion.full
)

initialCommands in Compile += """
  import scalaz._
  import Scalaz._
  import scala.math._
  import scala.swing.Swing._
  import org.scala_tools.time.Imports._
  import org.sfree.chart.Charting._
"""

initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile

initialCommands in Compile in console += """
  import grid._
  import grid.Accounting._
"""

