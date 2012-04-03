package grid

object EfficiencyByGroup extends EfficiencyApp {
  (dispatched groupBy { _.user.gid } efficiency).toList sortBy { _._3 } map formatted foreach println
}
