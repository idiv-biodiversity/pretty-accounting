package grid

import org.specs2._

class AccountingParserSpec extends Specification { def is = s2"""

  Accounting Parser Specification
    SGE 6.0u8 accounting entry                                                            $e1
    SGE 6.2u5 accounting entry                                                            $e2
    resource request contains colon                                                       $e3
                                                                                                 """
  // -----------------------------------------------------------------------------------------------
  // tests
  // -----------------------------------------------------------------------------------------------

  def e1 = Streaming.AccountingEntry.unapply(sixzeroueightEntry) must beSome
  def e2 = Streaming.AccountingEntry.unapply(sixtwoufiveEntry) must beSome
  def e3 = Streaming.AccountingEntry.unapply(resReqContainingColon) must beSome

  // -----------------------------------------------------------------------------------------------
  // utility functions
  // -----------------------------------------------------------------------------------------------

  def sixzeroueightEntry = """all.q:head.liclus.leipzig.ufz.de:oscartst:oscartst:pvmtest:1:sge:0:0:1163080982:1163080992:0:0:10:0:0:0.000000:0:0:0:0:18934:1:0:0.000000:0:0:0:0:316:131:NONE:defaultdepartment:pvm:1:0:0.000000:0.000000:0.000000:-pe pvm 1:0.000000:1.head:0.000000"""
  def sixtwoufiveEntry = """batch:node037:oekochem:mulliner:G03|OptFreqB3LYP6311Gdp_PCM_UA0_water_Y:142674:sge:0:1331104765:1331104765:1331124668:100:137:19903:0.008998:0.015997:0.000000:0:0:0:0:4055:0:0:0.000000:0:0:0:0:100:32:oekochem:oekochem:NONE:1:0:19882.720000:61564.294411:51.781509:-l h_rt=864000,highmem=0:0.000000:NONE:3468050432.000000:0:0"""
  def resReqContainingColon = """queue02:node072:wkdv:lberg:jobname:10245:sge:0:1303311181:1303311191:1303311491:0:0:300:0.002999:0.001999:0.000000:0:0:0:0:934:0:0:0.000000:0:0:0:0:49:9:wkdv:wkdv:NONE:1:0:0.004998:0.000000:0.000027:-l cpu=24:00:00:0.000000:NONE:12754944.000000:0:0"""

}
