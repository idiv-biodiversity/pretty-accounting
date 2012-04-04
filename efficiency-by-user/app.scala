package grid

object EfficiencyByUser extends EfficiencyApp {
  def name = "efficiency-by-user"

  (dispatched groupBy { _.user.uid } efficiency).toList sortBy { _._3 } map formatted foreach println
}
