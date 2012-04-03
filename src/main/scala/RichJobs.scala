package grid

object RichJobs extends RichJobs

trait RichJobs extends Filtering with RichTime with TypeImports {
  // TODO: jobs per 1.minute { _.slots }
  implicit def jobspimp(l: GenIterable[Job]) = new JobsPimp(l)
  implicit def jobcategorypimp[A](m: GenMap[A,GenIterable[Job]]) = new JobsCategoryPimp(m)

  class JobsPimp(jobs: GenIterable[Job]) {
    def toTimeslots(f: Job => Double)
                   (implicit interval: Option[Interval] = Some(thisYear)): Map[DateTime,Double] = {
      import scalaz.Scalaz._

      val ts: GenIterable[Map[DateTime,Double]] = for {
        job    <- interval map { implicit interval =>
                    jobs filter { isBetween(_) }
                  } getOrElse(jobs)
        start  =  job.time.start withSecondOfMinute 0
        end    =  job.time.end   withSecondOfMinute 0
        data   =  f(job)
      } yield (start to end by 1.minute map {
        _ -> data
      } toMap)

      ts.fold(Map())(_ |+| _)
    }

    def efficiency = Efficiency.efficiency(jobs)
  }

  class JobsCategoryPimp[A](groupedJobs: GenMap[A,GenIterable[Job]]) {
    def toTimeslots(f: Job => Double)
                   (implicit interval: Option[Interval] = Some(thisYear)): Map[A,Map[DateTime,Double]] =
      // TODO mapValues (requires newer scala)
      groupedJobs.map(kv => kv._1 -> kv._2.toTimeslots(f)).seq.toMap
  }
}
