package grid

import org.specs2._
import fs2.Stream

class AccountingParserSpec extends Specification with Parsing { def is = s2"""

  Accounting Entry Parser Specification

  parser should succeed for supported grid engine versions
    SGE 6.0                                                                               $e1
    SGE 6.2                                                                               $e2
    UGE 8.3                                                                               $e3
    UGE 8.4                                                                               $e4

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

  def e1 = eyes(Parser.sge60, sixzeroueightEntry)
  def e2 = eyes(Parser.sge62, sixtwoufiveEntry)
  def e3 = eyes(Parser.uge83, uge83)
  def e4 = eyes(Parser.uge84, uge84)

  def ec1 = eyes(Parser.sge62, resReqContainingColon)

  def b1 = eyes(Parser.uge83, broken1)
  def b2 = eyes(Parser.uge83, broken2)

  // -----------------------------------------------------------------------------------------------
  // utility functions
  // -----------------------------------------------------------------------------------------------

  def sixzeroueightEntry = """all.q:head.liclus.leipzig.ufz.de:group:user:pvmtest:1:sge:0:0:1163080982:1163080992:0:0:10:0:0:0.000000:0:0:0:0:18934:1:0:0.000000:0:0:0:0:316:131:NONE:defaultdepartment:pvm:1:0:0.000000:0.000000:0.000000:-pe pvm 1:0.000000:1.head:0.000000"""

  def sixtwoufiveEntry = """batch:node037:group:user:G03|OptFreqB3LYP6311Gdp_PCM_UA0_water_Y:142674:sge:0:1331104765:1331104765:1331124668:100:137:19903:0.008998:0.015997:0.000000:0:0:0:0:4055:0:0:0.000000:0:0:0:0:100:32:group:group:NONE:1:0:19882.720000:61564.294411:51.781509:-l h_rt=864000,highmem=0:0.000000:NONE:3468050432.000000:0:0"""

  def uge83 = """ideve_120:node164:group:user:BMO2:870542:sge:10:1452177896152:1453100011880:1453100075983:100:137:64.103:0.014:0.009:5268:0:0:0:0:7755:0:0:0:0:0:0:0:140:15:project:idiv_muellnerriehl:smp:5:0:86.780:689.914110:0.076263:-U idiv_muellnerriehl -u user -l h_rt=2592000,h_vmem=8G,highmem=false -pe smp 1-10 -binding no_job_binding 0 0 0 0 no_explicit_binding:0.000000:NONE:8637100032:0:0:NONE:root@head1:0:0:idiv1.eve.ufz.de:NONE:qsub -pe smp -10 -N BMO2 beast182.sh MarcOut_2.xml:64.108000"""

  def uge84 = """all.q:node104:group:user:jsv-test:26:sge:0:1487593940295:1487593940560:1487593941592:0:0:1.032:0.009:0.001:6156:0:0:0:0:3726:0:0:0:0:0:0:0:56:20:project:department:NONE:1:0:0.010:0.000000:0.000000:-U idiv_gsu -u krausec -l h_rt=900,h_vmem=1G,highmem=false,r_gpu=false -binding linear_automatic 1 0 0 0 NULL:0.000000:NONE:0:0:0:NONE:NONE:0:0:idiv1.eve.ufz.de:NONE:qsub -N jsv-test -l h_rt=900,h_vmem=1G -binding linearÿ1 -b y /bin/sleep 1:1.235000:0"""

  def resReqContainingColon = """queue02:node072:group:user:jobname:10245:sge:0:1303311181:1303311191:1303311491:0:0:300:0.002999:0.001999:0.000000:0:0:0:0:934:0:0:0.000000:0:0:0:0:49:9:group:group:NONE:1:0:0.004998:0.000000:0.000027:-l cpu=24:00:00:0.000000:NONE:12754944.000000:0:0"""

  def broken1 = """UNKNOWN:UNKNOWN:group:user:DrawArea_user.sh:1176727:sge:0:0:0:0:21:0:0.000:0.000:0.000:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:project:group:NONE:1:0:0.000:0.000000:0.000000:-U group -u user -l h_rt=3600,h_vmem=1G,highmem=false -binding linear_automatic 1 0 0 0 NULL:0.000000:NONE:0:0:0:NONE:root@head1:0:0:idiv1.eve.ufz.de:NONE:qsub /home/user/DrawArea/DrawArea_user.sh 75x60+180+470:0.000000"""

  def broken2 = """(null):(null):group:user:allMis1:1595011:sge:0:0:0:0:19:1:0.000:0.000:0.000:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:project:idiv_schlegel:smp:4:0:0.000:0.000000:0.000000:-u user -l h_rt=1080000,h_vmem=18G,highmem=TRUE -pe smp 4 -binding no_job_binding 0 0 0 0 no_explicit_binding:0.000000:NONE:0:0:0:NONE:NONE:0:0:frontend1:NONE:qsub run_anoCar2.sh:296092.304000"""

}
