package grid

import util.matching.Regex

object Filtering extends Filtering

trait Filtering {

  /** Returns the regular expression for excluding group IDs. It gets set by the system property
    * `grid.exclude.gids`, defaults to `"root"`.
    */
  def ExcludeGIDsRegex = sys.props.getOrElse("grid.exclude.gids", "root").r

  /** Returns the grid engine epoch start. */
  private lazy val GridEngineEpoch = new DateTime("1970-01-01T01:00:00.000+01:00")

  /** Returns a function that filters jobs by their group being not in the regex. */
  val gids: Regex ⇒ Job ⇒ Boolean = (exclude: Regex) ⇒ (j: Job) ⇒ exclude.unapplySeq(j.user.gid).isEmpty

  /** Returns a function that filters jobs by whether they were successful. */
  val successful: Job ⇒ Boolean = (j: Job) ⇒ j.status.successful

  /** Returns a function that filters jobs by whether they were unsuccessful. */
  val failed: Job ⇒ Boolean = (j: Job) ⇒ j.status.failed

  /** Returns a function that filters jobs by whether they were parallel. */
  val parallel: Job ⇒ Boolean = (j: Job) ⇒ j.parallelEnvironment.isDefined

  /** Returns a function that filters jobs by whether they were run somewhere in the interval. */
  val isBetween: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
    (j.time.end isAfter interval.start) && (j.time.start isBefore interval.end)

  /** Returns a function that filters jobs by whether they were started somewhere in the interval. */
  val startedBetween: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
    (j.time.start isAfter interval.start) && (j.time.start isBefore interval.end)

  /** Returns a function that filters jobs by whether they ended somewhere in the interval. */
  val endedBetween: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
    (j.time.end isAfter interval.start) && (j.time.end isBefore interval.end)

  /** Returns a function that filters jobs by their queue and node being non empty and their
    * submission time being sometime else than epoch.
    */
  val realJob: Job ⇒ Boolean = (j: Job) ⇒
    j.queue.nonEmpty && j.node.nonEmpty  && (j.time.submission != GridEngineEpoch)

  /** Returns a function that filters jobs by their wallclock time being positive. */
  val isDispatched: Job ⇒ Boolean = (j: Job) ⇒ j.res.wctime > 0

  /** Returns a function that applies both `gids` and `realJob`. */
  val combined: Regex ⇒ Job ⇒ Boolean = (exclude: Regex) ⇒ (j: Job) ⇒
    gids(exclude)(j) && realJob(j)

}
