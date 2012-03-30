package grid

import Job._

object AccountingEntry {
  private val UNKNOWN = "UNKNOWN"
  private val NONE    = "NONE"

  //                                        queue   host   u(gid   uid  )j(name  id   ) acc     prio   t(sub   start end  ) st(fail exit) ru(wc  utime stime maxr  ixr   ismr  idr   isr   minfl majfl nswap inblk oublk msgsn msgrc signs nvcsw nvcsw)a(proj  dep  ) pe      slots   taskid  cpu     mem     io      req  iow     pe_id   maxvmem ar(id  sub)
  private[grid] val accountingEntryRE = """([^:]+):([^:]+):([^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+:[^:]+):([^:]+:[^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):([^:]+):(.+):([^:]+):([^:]+):([^:]+):([^:]+:[^:]+)"""r

  def unapply(s: String) = s match {
    case accountingEntryRE(queue, node, user, jobId, account, priority, time, status, resourceUsage,
    acl, parallelEnvironment, slots, taskNumber, cpu, mem, io, resourceRequest, iow, parallelTaskId,
    maxvmem, reservation) =>

  // q host u() j() acc prio t() fail exit ru() acl() pe slots taskid cpu mem io resreq iow pe_id maxvmem ar()
    Some(Job(
      queue match { case UNKNOWN => None ; case queue => Some(queue) },
      node  match { case UNKNOWN => None ; case node  => Some(node)  },
      User(user),
      JobId(jobId, taskNumber),
      account,
      priority.toDouble,
      Time(time),
      Status(status),
      ResourceUsage(resourceUsage),
      Acl(acl),
      parallelEnvironment match { case NONE => None ; case s => Some(s) },
      slots.toInt,
      resourceRequest match { case NONE => None ; case s => Some(s) },
      parallelTaskId match { case NONE => None ; case s => Some(s.toInt) },
      Reservation(reservation)
    ))

    case _ => None
  }
}
