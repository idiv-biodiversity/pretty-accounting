import sbt._
import Keys._

import Dependencies._
import BuildSettings._

object BuildSettings {
  lazy val buildVersion      = "0.0.1-SNAPSHOT"
  lazy val buildScalaVersion = "2.9.1"

  lazy val baseSettings = Defaults.defaultSettings ++ Seq (
    version       := buildVersion,
    scalaVersion  := buildScalaVersion,
    resolvers    ++= Resolvers.combined
  )
}

object PrettyAccountingBuild extends Build {

  lazy val root = Project (
    id        = "pretty-accounting",
    base      = file("."),
    aggregate = Seq ( core,
      efficiencyByUser, efficiencyByGroup,
      slotsPerQueue, slotsSeqVsPar, slotsRunVsWait,
      jobsPerUser
    )
  )

  lazy val core = Project (
    id       = "pretty-accounting-core",
    base     = file("core"),
    settings = baseSettings ++ Seq (
      initialCommands in Compile += """
        import scalaz._
        import Scalaz._
        import scala.math._
        import scala.swing.Swing._
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
      libraryDependencies ++= Seq ( chart, pdf, swing, time, specs2, iocore, iofile, scalaz )
    )
  )

  lazy val efficiencyByUser = Project (
    id           = "efficiency-by-user",
    base         = file("efficiency-by-user"),
    dependencies = Seq ( core ),
    settings     = baseSettings
  )

  lazy val efficiencyByGroup = Project (
    id           = "efficiency-by-group",
    base         = file("efficiency-by-group"),
    dependencies = Seq ( core ),
    settings     = baseSettings
  )

  lazy val slotsPerQueue = Project (
    id           = "slots-per-queue",
    base         = file("slots-per-queue"),
    dependencies = Seq ( core ),
    settings     = baseSettings
  )

  lazy val slotsSeqVsPar = Project (
    id           = "slots-seq-vs-par",
    base         = file("slots-seq-vs-par"),
    dependencies = Seq ( core ),
    settings     = baseSettings
  )

  lazy val slotsRunVsWait = Project (
    id           = "slots-run-vs-wait",
    base         = file("slots-run-vs-wait"),
    dependencies = Seq ( core ),
    settings     = baseSettings
  )

  lazy val jobsPerUser = Project (
    id           = "jobs-per-user",
    base         = file("jobs-per-user"),
    dependencies = Seq ( core ),
    settings     = baseSettings
  )

}

object Resolvers {
  lazy val sonarels = "Sonatype Releases"   at "http://oss.sonatype.org/content/repositories/releases"
  lazy val sonasnap = "Sonatype Snapshots"  at "http://oss.sonatype.org/content/repositories/snapshots"
  lazy val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases"

  lazy val combined = Seq ( sonarels, sonasnap, typesafe )
}

object Dependencies {
  lazy val chart  = "org.jfree"                     %  "jfreechart"    % "1.0.14"
  lazy val pdf    = "com.lowagie"                   %  "itext"         % "2.1.7"
  lazy val swing  = "org.scala-lang"                %  "scala-swing"   % buildScalaVersion
  lazy val time   = "org.scala-tools.time"          %% "time"          % "0.5"
  lazy val scalaz = "org.scalaz"                    %% "scalaz-core"   % "6.0.4"
  lazy val iocore = "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.0"
  lazy val iofile = "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.0"
  lazy val specs2 = "org.specs2"                    %% "specs2"        % "1.9" % "test"
}
