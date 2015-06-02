package grid

import scalaz.Monoid
import scalaz.std.map._

trait EfficiencyApp extends AccountingApp {
  def defaultExtension = "txt"

  case class ADT(jobs: Long, slots: Long, eff: Double) {
    def +(that: ADT): ADT = ADT(jobs + that.jobs, slots + that.slots, eff + that.eff)
  }

  object ADT {
    def apply(job: Job): ADT = {
      val slots = job.slots.toLong
      ADT(1, slots, job.efficiency)
    }

    implicit val ADTMonoid: Monoid[ADT] = new Monoid[ADT] {
      val zero = ADT(0, 0, 0.0)
      def append(a: ADT, b: => ADT): ADT = a + b
    }
  }

  def formatted(kv: (String,ADT)): String = {
    val (key,adt) = kv
    val jobs = adt.jobs
    val slots = adt.slots
    val eff = (adt.eff / jobs * 10000).round / 100.0

    f"""$key%10s -> $jobs%10d jobs -> $slots%10d slots -> $eff%6.2f%% u+s"""
  }
}

object EfficiencyByUser extends EfficiencyApp {
  def name = "efficiency-by-user"

  filtered.runFoldMap {
    job => Map(job.user.uid -> ADT(job))
  }.run foreach {
    x => println(formatted(x))
  }
}

object EfficiencyByGroup extends EfficiencyApp {
  def name = "efficiency-by-group"

  filtered.runFoldMap {
    job => Map(job.user.gid -> ADT(job))
  }.run foreach {
    x => println(formatted(x))
  }
}

object EfficiencyByJob extends EfficiencyApp {
  def name = "efficiency-by-job"

  filtered.map({ job =>
    val eff = (((job.res.cputime) / job.slots / job.res.wctime) * 10000).round / 100.0
    (job.slots,job.res.utime,job.res.stime,job.res.cputime,job.res.wctime,eff)
  }).runLog.run.sortBy(_._6) foreach { x =>
    val (slots,utime,stime,cputime,wctime,eff) = x
    println(f"""$slots%10d slots   $utime%12.2f u   $stime%12.2f s   $cputime%12.2f cpu   $wctime%10d wc   $eff%6.2f eff""")
  }
}
