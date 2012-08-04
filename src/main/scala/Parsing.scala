package grid

object Parsing extends Parsing

trait Parsing extends Filtering with TypeImports {
  def accountingFilePath: String = sys.props get "grid.accounting.file" getOrElse {
    sys.env.getOrElse("SGE_ROOT", "/usr/local/sge") + "/default/common/accounting"
  }

  def accountingFileLines: GenIterable[String] =
    scalax.io.Resource.fromURL("file://" + accountingFilePath).lines().toIterable.par

  def raw(implicit lines: GenIterable[String] = accountingFileLines) = lines collect {
    case AccountingEntry(job) ⇒ job
  }

  def jobs(implicit lines: GenIterable[String] = accountingFileLines) =
    raw filter combined

  def dispatched = jobs filter isDispatched

  def linesNotMatching(implicit lines: GenIterable[String] = accountingFileLines) =
    lines filterNot { AccountingEntry.unapply(_).isDefined }

  object AccountingEntry {
    import Job._

    private val UNKNOWN = "UNKNOWN"
    private val NONE    = "NONE"

    //                                        queue   host   u(gid   uid  )j(name  id   ) acc     prio   t(sub   start end  ) st(fail exit) ru(wc  utime stime maxr  ixr   ismr  idr   isr   minfl majfl nswap inblk oublk msgsn msgrc signs nvcsw nvcsw)a(proj  dep  ) pe      slots   taskid  cpu     mem     io      req  iow     pe_id   maxvmem ar(id  sub)
    private[grid] val sixtwoufiveEntry   = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):([^:]+:[^:]+)"""r
    private[grid] val sixzeroueightEntry = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+)"""r

    def unapply(s: String) = s match {
      case sixtwoufiveEntry(queue, node, user, jobId, account, priority, time, status, res, acl,
      parallelEnvironment, slots, taskNumber, cpu, mem, io, resReq, iow, parallelTaskId, maxvmem,
      reservation) ⇒

      Some(Job(
        queue match { case UNKNOWN ⇒ None ; case queue ⇒ Some(queue) },
        node  match { case UNKNOWN ⇒ None ; case node  ⇒ Some(node)  },
        User(user),
        JobId(jobId, taskNumber),
        account,
        priority.toDouble,
        Time(time),
        Status(status),
        ResourceUsage(res,cpu,mem,maxvmem,io,iow),
        Acl(acl),
        parallelEnvironment match { case NONE ⇒ None ; case s ⇒ Some(s) },
        slots.toInt,
        resReq         match { case NONE ⇒ None ; case s ⇒ Some(s) },
        parallelTaskId match { case NONE ⇒ None ; case s ⇒ Some(s) },
        Reservation(reservation)
      ))

      case sixzeroueightEntry(queue, node, user, jobId, account, priority, time, status, res, acl,
      parallelEnvironment, slots, taskNumber, cpu, mem, io, resReq, iow, parallelTaskId, maxvmem) ⇒

      Some(Job(
        queue match { case UNKNOWN ⇒ None ; case queue ⇒ Some(queue) },
        node  match { case UNKNOWN ⇒ None ; case node  ⇒ Some(node)  },
        User(user),
        JobId(jobId, taskNumber),
        account,
        priority.toDouble,
        Time(time),
        Status(status),
        ResourceUsage(res,cpu,mem,maxvmem,io,iow),
        Acl(acl),
        parallelEnvironment match { case NONE ⇒ None ; case s ⇒ Some(s) },
        slots.toInt,
        resReq         match { case NONE ⇒ None ; case s ⇒ Some(s) },
        parallelTaskId match { case NONE ⇒ None ; case s ⇒ Some(s) },
        None
      ))

      case _ ⇒ None
    }
  }
}
