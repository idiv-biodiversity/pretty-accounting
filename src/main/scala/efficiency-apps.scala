package grid

trait EfficiencyApp extends AccountingApp {
  def defaultExtension = "txt"

  def formatted(t: Tuple4[String,Int,Double,Double]) =
    "%10s -> %10d jobs -> %6.2f%% u -> %6.2f%% u+s" format (
      t._1,                             //  group
      t._2,                             //  jobs
      (t._3 * 10000).round / 100.0,     //  utime
      (t._4 * 10000).round / 100.0      //  utime + stime
    )
}

object EfficiencyByUser extends EfficiencyApp {
  def name = "efficiency-by-user"

  dispatched.groupBy(_.user.uid).efficiency sortBy { _._3 } map formatted foreach println
}

object EfficiencyByGroup extends EfficiencyApp {
  def name = "efficiency-by-group"

  dispatched.groupBy(_.user.gid).efficiency sortBy { _._3 } map formatted foreach println
}
