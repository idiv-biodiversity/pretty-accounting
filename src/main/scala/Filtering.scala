package grid

import Accounting._

object Filtering extends Filtering

trait Filtering {
  lazy val ExcludeGIDs = "circular|root|wkdv|hpcworks|extusers".r

  lazy val nonAdminAndExternalGIDs = (j: Job) => ExcludeGIDs.unapplySeq(j.user.gid).isEmpty

  lazy val successful = (j: Job) => j.status.successful
  lazy val failed     = (j: Job) => j.status.failed

  lazy val parallel = (j: Job) => j.parallelEnvironment.isDefined

  lazy val epochstart = new DateTime("1970-01-01T01:00:00.000+01:00")

  def isBetween(j: Job)(implicit start: DateTime, end: DateTime) =
    (j.time.end isAfter start) && (j.time.start isBefore end)

  lazy val realJob = (j: Job) => j.queue.nonEmpty && j.node.nonEmpty  && (j.time.submission != epochstart)

  lazy val wasRunning = (j: Job) => j.resourceUsage.wallclock > 0

  lazy val combined = (j: Job) => nonAdminAndExternalGIDs(j) && realJob(j)
}
