package grid

object EfficiencyByGroup extends EfficiencyApp {
  def name = "efficiency-by-group"

  (dispatched groupBy { _.user.gid } efficiency).toList sortBy { _._3 } map formatted foreach println
}
