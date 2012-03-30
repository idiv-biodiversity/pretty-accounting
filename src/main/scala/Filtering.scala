package grid

import Accounting._

object Filtering extends Filtering

trait Filtering {
  lazy val ExcludeGIDs = "circular|root|wkdv|hpcworks|extusers".r
  lazy val epochstart  = new DateTime("1970-01-01T01:00:00.000+01:00")

  def nonAdminAndExternalGIDs(j: Job) = ExcludeGIDs.unapplySeq(j.user.gid).isEmpty

  def successful(j: Job) = j.status.successful
  def failed    (j: Job) = j.status.failed

  def parallel(j: Job) = j.parallelEnvironment.isDefined

  def isBetween(j: Job)(implicit start: DateTime, end: DateTime) =
    (j.time.end isAfter start) && (j.time.start isBefore end)

  def realJob(j: Job) = j.queue.nonEmpty && j.node.nonEmpty  && (j.time.submission != epochstart)

  def wasRunning(j: Job) = j.resourceUsage.wallclock > 0

  def combined(j: Job) = nonAdminAndExternalGIDs(j) && realJob(j)
}
