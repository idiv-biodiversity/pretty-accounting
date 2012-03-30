package grid

import Accounting._

object Efficiency extends Efficiency

trait Efficiency {
  def efficiency(jobs: GenSeq[Job]) =
    (jobs map { j => (j.resourceUsage.utime.toDouble / j.slots) } sum) /
    (jobs map { j =>  j.resourceUsage.wallclock.toLong } sum)

  def efficiencyGroupedBy[A](jobs: GenSeq[Job])(f: Job => A) = for {
    (group,jobs) <- jobs groupBy f
    numjobs      =  jobs.size
    eff          =  efficiency(jobs)
  } yield (group,numjobs,eff)

  def formatted(t: Triple[String,Int,Double]) =
    "%10s -> %10d -> %3d%%" format (t._1, t._2, (t._3 * 100).round)
}

object EfficiencyByUser extends App with Accounting {
  val jobs = filtered.par

  val efficiencies = efficiencyGroupedBy(jobs) { _.user.uid }

  efficiencies.toList sortBy { _._3 } foreach { p =>
    printf("%10s -> %10d -> %3d%%\n", p._1, p._2, (p._3 * 100).round)
  }
}

object EfficiencyByGroup extends App with Accounting {
  val jobs = filtered.par

  val efficiencies = efficiencyGroupedBy(jobs) { _.user.gid }

  efficiencies.toList sortBy { _._3 } foreach { p =>
    printf("%10s -> %10d -> %3d%%\n", p._1, p._2, (p._3 * 100).round)
  }
}
