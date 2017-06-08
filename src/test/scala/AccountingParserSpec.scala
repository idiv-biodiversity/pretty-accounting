package grid

import org.specs2._
import fs2.Stream

class AccountingParserSpec extends Specification with Parsing { def is = s2"""

  Accounting Entry Parser Specification

  UGE 8.4
    account                                                                     ${uge84.account}
    acl.department                                                              ${uge84.department}
    acl.project                                                                 ${uge84.project}
    id.job                                                                      ${uge84.jobid}
    id.name                                                                     ${uge84.jobname}
    id.task                                                                     ${uge84.taskid}
    node                                                                        ${uge84.node}
    queue                                                                       ${uge84.queue}
    time.submission                                                             ${uge84.submission}
    time.start                                                                  ${uge84.start}
    time.end                                                                    ${uge84.end}
    res.cputime                                                                 ${uge84.cputime}
    res.stime                                                                   ${uge84.stime}
    res.utime                                                                   ${uge84.utime}
    res.wctime                                                                  ${uge84.wctime}
    user.gid                                                                    ${uge84.gid}
    user.uid                                                                    ${uge84.uid}

  UGE 8.3
    account                                                                     ${uge83.account}
    acl.department                                                              ${uge83.department}
    acl.project                                                                 ${uge83.project}
    id.job                                                                      ${uge83.jobid}
    id.name                                                                     ${uge83.jobname}
    id.task                                                                     ${uge83.taskid}
    node                                                                        ${uge83.node}
    queue                                                                       ${uge83.queue}
    time.submission                                                             ${uge83.submission}
    time.start                                                                  ${uge83.start}
    time.end                                                                    ${uge83.end}
    res.cputime                                                                 ${uge83.cputime}
    res.stime                                                                   ${uge83.stime}
    res.utime                                                                   ${uge83.utime}
    res.wctime                                                                  ${uge83.wctime}
    user.gid                                                                    ${uge83.gid}
    user.uid                                                                    ${uge83.uid}

  SGE 6.2
    account                                                                     ${sge62.account}
    acl.department                                                              ${sge62.department}
    acl.project                                                                 ${sge62.project}
    id.job                                                                      ${sge62.jobid}
    id.name                                                                     ${sge62.jobname}
    id.task                                                                     ${sge62.taskid}
    node                                                                        ${sge62.node}
    queue                                                                       ${sge62.queue}
    time.submission                                                             ${sge62.submission}
    time.start                                                                  ${sge62.start}
    time.end                                                                    ${sge62.end}
    res.cputime                                                                 ${sge62.cputime}
    res.stime                                                                   ${sge62.stime}
    res.utime                                                                   ${sge62.utime}
    res.wctime                                                                  ${sge62.wctime}
    user.gid                                                                    ${sge62.gid}
    user.uid                                                                    ${sge62.uid}

  SGE 6.0
    account                                                                     ${sge60.account}
    acl.department                                                              ${sge60.department}
    acl.project                                                                 ${sge60.project}
    id.job                                                                      ${sge60.jobid}
    id.name                                                                     ${sge60.jobname}
    id.task                                                                     ${sge60.taskid}
    node                                                                        ${sge60.node}
    queue                                                                       ${sge60.queue}
    time.submission                                                             ${sge60.submission}
    time.start                                                                  ${sge60.start}
    time.end                                                                    ${sge60.end}
    res.cputime                                                                 ${sge60.cputime}
    res.stime                                                                   ${sge60.stime}
    res.utime                                                                   ${sge60.utime}
    res.wctime                                                                  ${sge60.wctime}
    user.gid                                                                    ${sge60.gid}
    user.uid                                                                    ${sge60.uid}

  parser should handle weird corner cases
    resource request contains colon                                                       $ec1

  parser should handle broken entries
    broken 1                                                                              $b1
    broken 2                                                                              $b2
                                                                                                 """
  // -----------------------------------------------------------------------------------------------
  // tests
  // -----------------------------------------------------------------------------------------------

  def e(p: Parser, line: String): Vector[Job] =
    p.parse(Stream(line)).runLog.unsafeRun

  def eyes(p: Parser, line: String) =
    e(p, line) must not be empty

  def ec1 = eyes(Parser.sge62, entry.resReqContainingColon)

  def b1 = eyes(Parser.uge83, entry.broken1)
  def b2 = eyes(Parser.uge83, entry.broken2)

  // -----------------------------------------------------------------------------------------------
  // utility functions
  // -----------------------------------------------------------------------------------------------

  object entry {

    def resReqContainingColon = """queue02:node072:group:user:jobname:10245:sge:0:1303311181:1303311191:1303311491:0:0:300:0.002999:0.001999:0.000000:0:0:0:0:934:0:0:0.000000:0:0:0:0:49:9:group:group:NONE:1:0:0.004998:0.000000:0.000027:-l cpu=24:00:00:0.000000:NONE:12754944.000000:0:0"""

    def broken1 = """UNKNOWN:UNKNOWN:group:user:DrawArea_user.sh:1176727:sge:0:0:0:0:21:0:0.000:0.000:0.000:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:project:group:NONE:1:0:0.000:0.000000:0.000000:-U group -u user -l h_rt=3600,h_vmem=1G,highmem=false -binding linear_automatic 1 0 0 0 NULL:0.000000:NONE:0:0:0:NONE:root@head1:0:0:idiv1.eve.ufz.de:NONE:qsub /home/user/DrawArea/DrawArea_user.sh 75x60+180+470:0.000000"""

    def broken2 = """(null):(null):group:user:allMis1:1595011:sge:0:0:0:0:19:1:0.000:0.000:0.000:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:project:idiv_schlegel:smp:4:0:0.000:0.000000:0.000000:-u user -l h_rt=1080000,h_vmem=18G,highmem=TRUE -pe smp 4 -binding no_job_binding 0 0 0 0 no_explicit_binding:0.000000:NONE:0:0:0:NONE:NONE:0:0:frontend1:NONE:qsub run_anoCar2.sh:296092.304000"""

  }

  trait ParserChecker {
    def parser: Parser
    def entry: String

    def check[A](f: Job => A): A = {
      val jobs = e(parser, entry)
      f(jobs.head)
    }

    def account =    check { _.account                   === "sge"           }
    def department = check { _.acl.department            === "department"    }
    def project =    check { _.acl.project               === Some("project") }
    def jobid =      check { _.id.job                    === 42              }
    def jobname =    check { _.id.name                   === "jobname"       }
    def taskid =     check { _.id.task                   === 0               }
    def node =       check { _.node                      === Some("node001") }
    def queue =      check { _.queue                     === Some("all.q")   }
    def gid =        check { _.user.gid                  === "group"         }
    def uid =        check { _.user.uid                  === "user"          }
  }

  object uge84 extends ParserChecker {
    val parser = Parser.uge84

    def wctime =     check { _.res.wctime                === 1.032           }
    def utime =      check { _.res.utime                 === 0.009           }
    def stime =      check { _.res.stime                 === 0.001           }
    def cputime =    check { _.res.cputime               === 0.010           }
    def submission = check { _.time.submission.getMillis === 1487593940295L  }
    def start =      check { _.time.start.getMillis      === 1487593940560L  }
    def end =        check { _.time.end.getMillis        === 1487593941592L  }

    val entry = """all.q:node001:group:user:jobname:42:sge:0:1487593940295:1487593940560:1487593941592:0:0:1.032:0.009:0.001:6156:0:0:0:0:3726:0:0:0:0:0:0:0:56:20:project:department:NONE:1:0:0.010:0.000000:0.000000:-U idiv_gsu -u krausec -l h_rt=900,h_vmem=1G,highmem=false,r_gpu=false -binding linear_automatic 1 0 0 0 NULL:0.000000:NONE:0:0:0:NONE:NONE:0:0:idiv1.eve.ufz.de:NONE:qsub -N jobname -l h_rt=900,h_vmem=1G -binding linearÿ1 -b y /bin/sleep 1:1.235000:0"""
  }

  object uge83 extends ParserChecker {
    val parser = Parser.uge83

    def wctime =     check { _.res.wctime                === 64.103            }
    def utime =      check { _.res.utime                 === 0.014             }
    def stime =      check { _.res.stime                 === 0.009             }
    def cputime =    check { _.res.cputime               === 86.780            }
    def submission = check { _.time.submission.getMillis === 1452177896152L    }
    def start =      check { _.time.start.getMillis      === 1453100011880L    }
    def end =        check { _.time.end.getMillis        === 1453100075983L    }

    val entry = """all.q:node001:group:user:jobname:42:sge:10:1452177896152:1453100011880:1453100075983:100:137:64.103:0.014:0.009:5268:0:0:0:0:7755:0:0:0:0:0:0:0:140:15:project:department:smp:5:0:86.780:689.914110:0.076263:-U department -u user -l h_rt=2592000,h_vmem=8G,highmem=false -pe smp 1-10 -binding no_job_binding 0 0 0 0 no_explicit_binding:0.000000:NONE:8637100032:0:0:NONE:root@head1:0:0:idiv1.eve.ufz.de:NONE:qsub -pe smp -10 -N jobname beast182.sh MarcOut_2.xml:64.108000"""
  }

  object sge62 extends ParserChecker {
    val parser = Parser.sge62

    def wctime =     check { _.res.wctime                === 19903.000       }
    def utime =      check { _.res.utime                 === 0.008998        }
    def stime =      check { _.res.stime                 === 0.015997        }
    def cputime =    check { _.res.cputime               === 19882.720       }
    def submission = check { _.time.submission.getMillis === 1331104765L     }
    def start =      check { _.time.start.getMillis      === 1331104765L     }
    def end =        check { _.time.end.getMillis        === 1331124668L     }

    val entry = """all.q:node001:group:user:jobname:42:sge:0:1331104765:1331104765:1331124668:100:137:19903:0.008998:0.015997:0.000000:0:0:0:0:4055:0:0:0.000000:0:0:0:0:100:32:project:department:NONE:1:0:19882.720000:61564.294411:51.781509:-l h_rt=864000,highmem=0:0.000000:NONE:3468050432.000000:0:0"""
  }

  object sge60 extends ParserChecker {
    val parser = Parser.sge60

    def wctime =     check { _.res.wctime                === 217020.000          }
    def utime =      check { _.res.utime                 === 216934.000          }
    def stime =      check { _.res.stime                 === 37.000              }
    def cputime =    check { _.res.cputime               === 216971.060          }
    def submission = check { _.time.submission.getMillis === 1313763459L         }
    def start =      check { _.time.start.getMillis      === 1313763463L         }
    def end =        check { _.time.end.getMillis        === 1313980483L         }

    val entry = """all.q:node001:group:user:jobname:42:sge:0:1313763459:1313763463:1313980483:0:0:217020:216934:37:0.000000:0:0:0:0:4020237:0:0:0.000000:0:0:0:0:786:114139:project:department:NONE:1:0:216971.060000:484359.414227:0.000000:-U long,ldap -q midseq:0.000000:NONE:2672427008.000000"""
  }

}
