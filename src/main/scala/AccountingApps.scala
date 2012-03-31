package grid

trait AccountingApp extends App with Accounting {
  def filtered = jobs filter successful filter { _.res.wctime > 0 }
}

object EfficiencyByUser extends AccountingApp {
  (efficiencyGroupedBy(filtered) { _.user.uid } toList) sortBy { _._3 } map formatted foreach println
}

object EfficiencyByGroup extends AccountingApp {
  (efficiencyGroupedBy(filtered) { _.user.gid } toList) sortBy { _._3 } map formatted foreach println
}
