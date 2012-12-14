package grid

object Categorizing extends Categorizing

trait Categorizing extends Filtering with RichTime {

  /** Returns a function that categorizes jobs by whether they are sequential or parallel. */
  val SeqVsPar: Job ⇒ String = (job: Job) ⇒ if (parallel(job)) "parallel".localized else "sequential".localized

  /** Returns a function that categorizes jobs by their project. */
  val project: Job ⇒ String = (job: Job) ⇒ job.acl.project

  /** Returns a function that categorizes jobs by their department. */
  val department: Job ⇒ String = (job: Job) ⇒ job.acl.department

  /** Returns a function that categorizes jobs by the month of their submission. */
  val month_of_submission: Job ⇒ LocalDate = (job: Job) ⇒ job.time.submission.toLocalDate.withDayOfMonth(1)

  /** Returns a function that categorizes jobs by the quarterly period they were started. */
  val quarter_of_start: Job ⇒ String = (job: Job) ⇒ {
    val quarter = job.time.start.getQuarterOfYear
    val year    = job.time.start.getYear

    s"$year $quarter"
  }

  /** Returns a function that categorizes jobs by their UID. */
  val owner: Job ⇒ String = (job: Job) ⇒ job.user.uid

  /** Returns a function that categorizes jobs by their GID. */
  val group: Job ⇒ String = (job: Job) ⇒ job.user.gid

}
