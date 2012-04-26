package grid

object RichJobs extends RichJobs

trait RichJobs extends Filtering with RichTime with TypeImports {
  // TODO toTimeslots with variable slot period
  implicit def jobspimp(l: GenIterable[Job]) = new JobsPimp(l)
  implicit def jobcategorypimp[A](m: GenMap[A,GenIterable[Job]]) = new JobsCategoryPimp(m)

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

      val (pending,running): (GenIterable[Map[DateTime,Int]],GenIterable[Map[DateTime,Int]]) = for {
        job    <- interval map { implicit interval =>
                    jobs filter { isBetween(_) }
                  } getOrElse(jobs)
        sub    =  job.time.submission withSecondOfMinute 0
        start  =  job.time.start      withSecondOfMinute 0
        end    =  job.time.end        withSecondOfMinute 0
        slots  =  job.slots
      } yield (sub   to start by 1.minute map { _ -> slots } toMap) ->
              (start to end   by 1.minute map { _ -> slots } toMap)

      Map (
        "pending" -> pending.fold(Map())(_ |+| _),
        "running" -> running.fold(Map())(_ |+| _)
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
      // TODO mapValues (requires newer scala)
      groupedJobs.map(kv => kv._1 -> kv._2.toTimeslots(f)).seq.toMap

    def efficiency(implicit interval: Option[Interval]) = for {
      (group,jobs) <- groupedJobs
      numjobs      =  jobs.size
      ueff         =  jobs.efficiency { j => (j.res.utime   / j.slots) }
      useff        =  jobs.efficiency { j => (j.res.cputime / j.slots) }
    } yield (group,numjobs,ueff,useff)
  }
}
