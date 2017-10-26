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

  /** Returns the grid engine epoch start. */
  private lazy val GridEngineEpoch = new DateTime("1970-01-01T01:00:00.000+01:00")

  /** Contains filters based on excluding jobs by their users name or group.
    *
    * {{{
    * jobs filter TODO
    * }}}
    *
    * @group filter-misc
    */
  object exclude {

    def apply(j: Job)(implicit conf: Config): Boolean =
      gids(j) && uids(j)

    /** Returns a function that filters jobs by their group.
      *
      * @group filter-misc
      */
    def gids(j: Job)(implicit conf: Config): Boolean =
      !(conf.exclude.gids contains j.user.gid)

    /** Returns a function that filters jobs by their owner.
      *
      * @group filter-misc
      */
    def uids(j: Job)(implicit conf: Config): Boolean =
      !(conf.exclude.uids contains j.user.uid)

  }

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

    /** Returns a function that filters jobs by whether they were started
      * somewhere in the interval.
      */
    def between(interval: Interval): Job ⇒ Boolean = (j: Job) ⇒
      started.after(interval.start)(j) && started.before(interval.end)(j)

    /** Returns a function that filters jobs by whether they were started
      * somewhere in the interval.
      *
      * If start is not given, the beginning of the universe is assumed. If end
      * is not given, the end of the universe is assumed.
      */
    def between(config: Config): Job ⇒ Boolean = (job: Job) ⇒
      config.start.fold(true)(job.time.start >= _) &&
      config.end.fold(true)(job.time.start < _)

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
  val realJob: Job ⇒ Boolean = (j: Job) ⇒ {
    j.queue.nonEmpty &&
    j.node.nonEmpty &&
    j.time.submission != GridEngineEpoch &&
    j.time.start != GridEngineEpoch &&
    j.time.end != GridEngineEpoch
  }

  /** Returns a function that filters jobs by their wallclock time being positive.
    *
    * @group filter-misc
    */
  val isDispatched: Job ⇒ Boolean = (j: Job) ⇒ j.res.wctime > 0

  /** Returns a function that applies `realJob`, `exclude` and `started.between`.
    *
    * @group filter-misc
    */
  def combined(j: Job)(implicit conf: Config): Boolean =
    realJob(j) && exclude(j) && started.between(conf)(j)

  /** Returns a function that returns `true` for all jobs.
    *
    * @group filter-misc
    */
  val allJobs: Job ⇒ Boolean = (_: Job) ⇒ true

}
