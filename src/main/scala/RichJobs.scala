package grid

object RichJobs extends RichJobs

trait RichJobs extends RichCharting with Filtering with TimeImplicits {
  // TODO: jobs per 1.minute { _.slots }
  implicit def jobspimp(l: GenIterable[Job]) = new JobsPimp(l)
  implicit def jobcategorypimp[A](m: GenMap[A,GenIterable[Job]]) = new JobsCategoryPimp(m)

  def timeslots(jobs: GenIterable[Job])(f: Job => Double)
    (implicit start: DateTime = DateTime.now.withDayOfYear(1).withMillisOfDay(0),
              end:   DateTime = DateTime.now): Map[DateTime,Double] = {
    import scalaz.Scalaz._

    val ts: GenIterable[Map[DateTime,Double]] = for {
      job    <- jobs filter { isBetween(_)(start,end) }
      start  =  job.time.start withSecondOfMinute 0
      end    =  job.time.end   withSecondOfMinute 0
      data   =  f(job)
    } yield (start to end by 1.minute map { //if (time isAfter start) && (time isBefore end)
      _ -> data
    } toMap)

    ts.fold(Map())(_ |+| _)
  }

  class JobsPimp(jobs: GenIterable[Job]) {
    def perMinute(f: Job => Double)
      (implicit start: DateTime = DateTime.now.withDayOfYear(1).withMillisOfDay(0),
                end:   DateTime = DateTime.now) = {
      val dataset = new TimeSeries("")
      timeslots(jobs)(f)(start,end).seq foreach { kv =>
        dataset.add(kv._1,kv._2)
      }
      dataset
    }

    def efficiency = Efficiency.efficiency(jobs)
  }

  class JobsCategoryPimp[A](groupedJobs: GenMap[A,GenIterable[Job]]) {
    def perMinute(f: Job => Double)
      (implicit start: DateTime = DateTime.now.withDayOfYear(1).withMillisOfDay(0),
                end:   DateTime = DateTime.now) = {
      val dataset = new TimeTableXYDataset()

      for {
        group         <-  groupedJobs.seq map { _._1 } // replace with keys (requires newer scala)
        jobs          =   groupedJobs(group)
        (time,value)  <-  timeslots(jobs)(f)(start,end)
      } dataset add (time, value, group.toString)

      dataset
    }

    def perMinuteWithAverage(f: Job => Double)
      (implicit start: DateTime = DateTime.now.withDayOfYear(1).withMillisOfDay(0),
                end:   DateTime = DateTime.now,
                avg:   Int) = {
      import org.jfree.data.time.MovingAverage._
      import scala.collection.JavaConversions._

      val dataset = new TimeTableXYDataset()

      for {
        group   <-  groupedJobs.seq map { _._1 } // replace with keys (requires newer scala)
        jobs    =   groupedJobs(group)
        series  =   createMovingAverage(jobs.perMinute(f)(start,end), "", avg, avg)
        item    <-  series.getItems map { _.asInstanceOf[TimeSeriesDataItem] }
        period  =   item.getPeriod
        value   =   item.getValue.doubleValue
      } dataset add (period, value, group.toString)

      dataset
    }
  }
}
