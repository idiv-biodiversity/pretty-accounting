package grid

object RichJobs extends RichJobs

trait RichJobs extends Filtering with RichTime with TypeImports {
  // TODO toTimeslots with variable slot period
  implicit def jobspimp(l: GenIterable[Job]) = new JobsPimp(l)
  implicit def jobcategorypimp[A](m: GenMap[A,GenIterable[Job]]) = new JobsCategoryPimp(m)

  def timeslots[A](jobs: GenIterable[Job])
                  (f: Job => A, start: Job => DateTime, end: Job => DateTime)
                  : GenIterable[Map[DateTime,A]] = for {
    job <- jobs
    s   =  start(job) withSecondOfMinute 0
    e   =  end(job)   withSecondOfMinute 0
    d   =  f(job)
  } yield (s to e by 1.minute map { _ -> d } toMap)

  class JobsPimp(jobs: GenIterable[Job]) {
    def toTimeslots(f: Job => Double)
                   (implicit interval: Option[Interval]): Map[DateTime,Double] = {
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

    def toPendingVsRunning(implicit interval: Option[Interval]): Map[String,Map[DateTime,Int]] = {
      import scalaz.Scalaz._

      val filtered = interval map { implicit interval =>
        jobs filter { isBetween(_) }
      } getOrElse { jobs }

      Map (
        "pending" -> timeslots(filtered)(_.slots,_.time.submission,_.time.start).fold(Map())(_ |+| _),
        "running" -> timeslots(filtered)(_.slots,_.time.start     ,_.time.end  ).fold(Map())(_ |+| _)
      )
    }

    def efficiency(f: Job => Double)(implicit interval: Option[Interval]) = {
      val filtered = interval map { implicit interval =>
        jobs filter { isBetween(_) }
      } getOrElse(jobs)

      val fSum      = filtered map f sum
      val wctimeSum = filtered map { _.res.wctime } sum

      fSum / wctimeSum
    }
  }

  class JobsCategoryPimp[A](groupedJobs: GenMap[A,GenIterable[Job]]) {
    def toTimeslots(f: Job => Double)
                   (implicit interval: Option[Interval]): Map[A,Map[DateTime,Double]] =
      groupedJobs.mapValues(_.toTimeslots(f)).seq.toMap

    def efficiency(implicit interval: Option[Interval]) = for {
      (group,jobs) <- groupedJobs
      numjobs      =  jobs.size
      ueff         =  jobs.efficiency { j => (j.res.utime   / j.slots) }
      useff        =  jobs.efficiency { j => (j.res.cputime / j.slots) }
    } yield (group,numjobs,ueff,useff)
  }
}
