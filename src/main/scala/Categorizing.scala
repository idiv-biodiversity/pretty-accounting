package grid

object Categorizing extends Categorizing

trait Categorizing extends Filtering {

  /** Returns a function that categorizes jobs by whether they are sequential or parallel. */
  val SeqVsPar = (job: Job) ⇒ if (parallel(job)) "parallel".localized else "sequential".localized

}
