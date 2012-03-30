package grid

import scala.collection.JavaConversions._

import Accounting._

object RichJobs extends RichJobs

trait RichJobs extends Filtering {
  // TODO: jobs per 1.minute { _.slots }
  implicit def jobspimp(l: GenSeq[Job]) = new JobsPimp(l)
  implicit def jobcategorypimp[A](m: Map[A,GenSeq[Job]]) = new JobsCategoryPimp(m)

  class JobsPimp(jobs: GenSeq[Job]) {
    def perMinute[A](name: A)(f: Job => Double)
      (implicit start: DateTime = DateTime.now.withDayOfYear(1).withMillisOfDay(0),
                end:   DateTime = DateTime.now,
                ord:   Ordering[A]) = {
      import scala.math.Ordered._
      val dataset = new TimeSeries(name)

      for {
        job    <- jobs filter { isBetween(_)(start,end) }
        start  =  job.time.start
        end    =  job.time.end
        data   =  f(job)
        time   <- start to end by 1.minute //if (time isAfter start) && (time isBefore end)
        old    =  dataset getValue time
      } dataset addOrUpdate (time, if (old == null) data else (data + old.doubleValue))

      dataset
    }

    def efficiency = Efficiency.efficiency(jobs)
  }

  class JobsCategoryPimp[A](categories: Map[A,GenSeq[Job]]) {
    def perMinute(name: String)(f: Job => Double)
      (implicit start: DateTime = DateTime.now.withDayOfYear(1).withMillisOfDay(0),
                end:   DateTime = DateTime.now,
                ord:   Ordering[A]) = {
      val dataset = new TimeTableXYDataset()

      for {
        category <- categories.keys
        jobs     =  categories(category)
        series   =  jobs.perMinute(category)(f)(start,end,ord)
        item     <- series.getItems map { _.asInstanceOf[TimeSeriesDataItem] }
        period   =  item.getPeriod
        value    =  item.getValue.doubleValue
      } dataset add (period, value, category.toString)

      dataset
    }
  }
}
