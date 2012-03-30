package grid

import Accounting._

object Efficiency extends Efficiency

trait Efficiency {
  def efficiency(jobs: GenIterable[Job]) =
    (jobs map { j => (j.resourceUsage.utime.toDouble / j.slots) } sum) /
    (jobs map { j =>  j.resourceUsage.wallclock.toLong } sum)

  def efficiencyGroupedBy[A](jobs: GenIterable[Job])(f: Job => A) = for {
    (group,jobs) <- jobs groupBy f
    numjobs      =  jobs.size
    eff          =  efficiency(jobs)
  } yield (group,numjobs,eff)

  def formatted(t: Triple[String,Int,Double]) =
    "%10s -> %10d -> %3d%%" format (t._1, t._2, (t._3 * 100).round)
}
