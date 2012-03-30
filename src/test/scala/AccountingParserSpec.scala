package grid

import org.specs2._

class AccountingParserSpec extends Specification { def is =

  // -----------------------------------------------------------------------
  // fragments
  // -----------------------------------------------------------------------

  "Accounting Parser specification"                                           ^
    "accounting entry 2 job"               ! e1                               ^
    "resource request contains colon"      ! e2                               ^
                                                                            end
  // -----------------------------------------------------------------------
  // tests
  // -----------------------------------------------------------------------

  def e1 = AccountingEntry.unapply(validAccountingEntry) must beSome
  def e2 = AccountingEntry.unapply(entryResourceRequestContainsColon) must beSome

  // -----------------------------------------------------------------------
  // utility functions
  // -----------------------------------------------------------------------

  def validAccountingEntry = """batch:node037:oekochem:mulliner:G03|OptFreqB3LYP6311Gdp_PCM_UA0_water_Y:142674:sge:0:1331104765:1331104765:1331124668:100:137:19903:0.008998:0.015997:0.000000:0:0:0:0:4055:0:0:0.000000:0:0:0:0:100:32:oekochem:oekochem:NONE:1:0:19882.720000:61564.294411:51.781509:-l h_rt=864000,highmem=0:0.000000:NONE:3468050432.000000:0:0"""
  def entryResourceRequestContainsColon = """queue02:node072:wkdv:lberg:jobname:10245:sge:0:1303311181:1303311191:1303311491:0:0:300:0.002999:0.001999:0.000000:0:0:0:0:934:0:0:0.000000:0:0:0:0:49:9:wkdv:wkdv:NONE:1:0:0.004998:0.000000:0.000027:-l cpu=24:00:00:0.000000:NONE:12754944.000000:0:0"""

}
