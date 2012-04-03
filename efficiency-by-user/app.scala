package grid

object EfficiencyByUser extends EfficiencyApp {
  (dispatched groupBy { _.user.uid } efficiency).toList sortBy { _._3 } map formatted foreach println
}
