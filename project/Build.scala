import sbt._
import Keys._

import Dependencies._
import BuildSettings._

object BuildSettings {
  val buildVersion      = "0.0.1-SNAPSHOT"
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    version              := buildVersion,
    scalaVersion         := buildScalaVersion,
    resolvers           ++= Seq (
      "releases"              at "http://oss.sonatype.org/content/repositories/releases",
      "snapshots"             at "http://oss.sonatype.org/content/repositories/snapshots",
      "ScalaNLP Maven2"       at "http://repo.scalanlp.org/repo"
    )
  )
}

object PrettyAccountingBuild extends Build {

  lazy val root = Project (
    id       = "pretty-accounting",
    base     = file("."),
    settings = buildSettings ++ Seq (
      initialCommands in Compile += """
        import scala.math._
        import scala.swing.Swing._
        import scala.sys.process._
        import org.scala_tools.time.Imports._
        import org.jfree.chart._
        import org.jfree.chart.ChartFactory._
        import org.jfree.data.time._
        import org.jfree.data.time.MovingAverage._
      """,
      initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile,
      initialCommands in Compile in console += """
        import grid.Accounting._
      """,
      libraryDependencies ++= Seq ( chart, pdf, swing, time, specs2 )
    )
  )

}

object Dependencies {
  lazy val chart  = "org.jfree"            %  "jfreechart"  % "1.0.14"
  lazy val pdf    = "com.lowagie"          %  "itext"       % "2.1.7"
  lazy val swing  = "org.scala-lang"       %  "scala-swing" % buildScalaVersion
  lazy val time   = "org.scala-tools.time" %% "time"        % "0.5"
  lazy val specs2 = "org.specs2"           %% "specs2"      % "1.8.2" % "test"
}
