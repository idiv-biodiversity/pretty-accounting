package grid

object JobsPerUser extends ChartingApp {
  def name = "jobs-per-user"

  implicit val title = "Jobs per User"
  implicit val dataset: CategoryDataset = interval map { implicit interval =>
    dispatched filter { isBetween(_) }
  } getOrElse {
    dispatched
  } groupBy {
    _.user.uid
  } mapValues {
    _.size
  }

  createLabelledBarChart saveAs extension
}
