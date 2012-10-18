package grid

object Categorizing extends Categorizing

trait Categorizing extends Filtering {

  /** Returns a function that categorizes jobs by whether they are sequential or parallel. */
  val SeqVsPar = (job: Job) ⇒ if (parallel(job)) "parallel".localized else "sequential".localized

  /** Returns a function that categorizes jobs by their project. */
  val project = (job: Job) ⇒ job.acl.project

  /** Returns a function that categorizes jobs by their department. */
  val department: Job ⇒ String = (job: Job) ⇒ job.acl.department

  /** Returns a function that categorizes jobs by the month of their submission. */
  val month_of_submission = (job: Job) ⇒ job.time.submission.toLocalDate.withDayOfMonth(1)

  /** Returns a function that categorizes jobs by their UID. */
  val owner = (job: Job) ⇒ job.user.uid

  /** Returns a function that categorizes jobs by their GID. */
  val group = (job: Job) ⇒ job.user.gid

}
