package grid

object Efficiency extends Efficiency

trait Efficiency extends TypeImports {
  def efficiency(jobs: GenIterable[Job]) = {
    val utimeSum  = jobs map { j => (j.res.utime / j.slots) } sum
    val wctimeSum = jobs map { _.res.wctime } sum

    utimeSum / wctimeSum
  }

  def efficiencyWithStime(jobs: GenIterable[Job]) = {
    val ustimeSum = jobs map { j => (j.res.cputime / j.slots) } sum
    val wctimeSum = jobs map { _.res.wctime } sum

    ustimeSum / wctimeSum
  }

  def efficiencyGroupedBy[A](jobs: GenIterable[Job])(f: Job => A) = for {
    (group,jobs) <- jobs groupBy f
    numjobs      =  jobs.size
    ueff         =  efficiency(jobs)
    useff        =  efficiencyWithStime(jobs)
  } yield (group,numjobs,ueff,useff)

  def formatted(t: Tuple4[String,Int,Double,Double]) =
    "%10s -> %10d jobs -> %6.2f%% u -> %6.2f%% u+s" format (
      t._1,                             //  group
      t._2,                             //  jobs
      (t._3 * 10000).round / 100.0,     //  utime
      (t._4 * 10000).round / 100.0      //  utime + stime
    )
}
