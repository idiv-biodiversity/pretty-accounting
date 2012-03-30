package grid

trait AccountingApp extends App with Accounting {
  def jobs = filtered.par
}

object EfficiencyByUser extends AccountingApp {
  (efficiencyGroupedBy(jobs) { _.user.uid } toList) sortBy { _._3 } map formatted foreach println
}

object EfficiencyByGroup extends AccountingApp {
  (efficiencyGroupedBy(jobs) { _.user.gid } toList) sortBy { _._3 } map formatted foreach println
}
