package grid

import util.matching.Regex

object Filtering extends Filtering

/** Contains job filters. These filters are intended to be used with collections of jobs:
  *
  * {{{
  * val jobs: Seq[Job] = ???
  *
  * jobs filter (submitted between interval)
  * jobs filter successful
  * }}}
  *
  * @groupname filter-status Status-Based Filtering
  * @groupname filter-time Time-Based Filtering
  * @groupname filter-misc Miscellaneous Filtering
  */
trait Filtering {

  /** Returns the regular expression for excluding group IDs. It gets set by the system property
    * `grid.exclude.gids`, defaults to `"root"`.
    *
    * @group filter-misc
    */
  // TODO move to config
  def ExcludeGIDsRegex = sys.props.getOrElse("grid.exclude.gids", "root").r

  /** Returns the grid engine epoch start. */
  private lazy val GridEngineEpoch = new DateTime("1970-01-01T01:00:00.000+01:00")

  /** Returns a function that filters jobs by their group being not in the regex.
    *
    * @group filter-misc
    */
  val gids: Regex ⇒ Job ⇒ Boolean = (exclude: Regex) ⇒ (j: Job) ⇒ exclude.unapplySeq(j.user.gid).isEmpty

  /** Returns a function that filters jobs by whether they were successful.
    *
    * @group filter-status
    */
  val successful: Job ⇒ Boolean = (j: Job) ⇒ j.status.successful

  /** Returns a function that filters jobs by whether they were unsuccessful.
    *
    * @group filter-status
    */
  val failed: Job ⇒ Boolean = (j: Job) ⇒ j.status.failed

  /** Returns a function that filters jobs by whether they were parallel.
    *
    * @group filter-misc
    */
  val parallel: Job ⇒ Boolean = (j: Job) ⇒ j.parallelEnvironment.isDefined

  /** Contains filters based on job submission time.
    *
    * {{{
    * jobs filter (submitted between interval)
    * }}}
    *
    * @group filter-time
    */
  object submitted {

    /** Returns a function that filters jobs by whether they were submitted after the given date. */
    val after: DateTime ⇒ Job ⇒ Boolean = (date: DateTime) ⇒ (j: Job) ⇒
      j.time.submission isAfter date

    /** Returns a function that filters jobs by whether they were submitted before the given date. */
    val before: DateTime ⇒ Job ⇒ Boolean = (date: DateTime) ⇒ (j: Job) ⇒
      j.time.submission isBefore date

    /** Returns a function that filters jobs by whether they were submitted somewhere in the interval. */
    val between: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
      submitted.after(interval.start)(j) && submitted.before(interval.end)(j)

  }

  /** Contains filters based on job start time.
    *
    * {{{
    * jobs filter (started between interval)
    * }}}
    *
    * @group filter-time
    */
  object started {

    /** Returns a function that filters jobs by whether they started after the given date. */
    val after: DateTime ⇒ Job ⇒ Boolean = (date: DateTime) ⇒ (j: Job) ⇒
      j.time.start isAfter date

    /** Returns a function that filters jobs by whether they started before the given date. */
    val before: DateTime ⇒ Job ⇒ Boolean = (date: DateTime) ⇒ (j: Job) ⇒
      j.time.start isBefore date

    /** Returns a function that filters jobs by whether they were started somewhere in the interval. */
    val between: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
      started.after(interval.start)(j) && started.before(interval.end)(j)

  }

  /** Contains filters based on job end time.
    *
    * {{{
    * jobs filter (ended between interval)
    * }}}
    *
    * @group filter-time
    */
  object ended {

    /** Returns a function that filters jobs by whether they ended after the given date. */
    val after: DateTime ⇒ Job ⇒ Boolean = (date: DateTime) ⇒ (j: Job) ⇒
      j.time.end isAfter date

    /** Returns a function that filters jobs by whether they ended before the given date. */
    val before: DateTime ⇒ Job ⇒ Boolean = (date: DateTime) ⇒ (j: Job) ⇒
      j.time.end isBefore date

    /** Returns a function that filters jobs by whether they ended somewhere in the interval. */
    val between: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
      ended.after(interval.start)(j) && ended.before(interval.end)(j)

  }

  /** Returns a function that filters jobs by whether they were run somewhere in the interval.
    *
    * @group filter-time
    */
  val isBetween: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
    ended.after(interval.start)(j) && started.before(interval.end)(j)

  /** Returns a function that filters jobs by whether they were run completely within the interval.
    *
    * @group filter-time
    */
  val startedAndEndedBetween: Interval ⇒ Job ⇒ Boolean = (interval: Interval) ⇒ (j: Job) ⇒
    started.after(interval.start)(j) && ended.before(interval.end)(j)

  /** Returns a function that filters jobs by their queue and node being non empty and their
    * submission time being sometime else than epoch.
    *
    * @group filter-misc
    */
  val realJob: Job ⇒ Boolean = (j: Job) ⇒
    j.queue.nonEmpty && j.node.nonEmpty  && (j.time.submission != GridEngineEpoch)

  /** Returns a function that filters jobs by their wallclock time being positive.
    *
    * @group filter-misc
    */
  val isDispatched: Job ⇒ Boolean = (j: Job) ⇒ j.res.wctime > 0

  /** Returns a function that applies both `gids` and `realJob`.
    *
    * @group filter-misc
    */
  val combined: Regex ⇒ Job ⇒ Boolean = (exclude: Regex) ⇒ (j: Job) ⇒
    gids(exclude)(j) && realJob(j)

  /** Returns a function that returns true for all jobs.
    *
    * @group filter-misc
    */
  val allJobs: Job ⇒ Boolean = (_: Job) ⇒ true

}
