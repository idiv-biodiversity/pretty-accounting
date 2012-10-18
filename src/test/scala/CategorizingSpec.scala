package grid

import org.specs2._

class CategorizerSpec extends Specification with Accounting { def is =

  // -----------------------------------------------------------------------------------------------
  // fragments
  // -----------------------------------------------------------------------------------------------

  "Categorizer Function Specification"                                                             ^
    "month of submission"                                                       ! e1               ^
    "project"                                                                   ! e2               ^
    "department"                                                                ! e3               ^
    "sequential vs parallel"                                                    ! e4               ^
    "group"                                                                     ! e5               ^
    "owner"                                                                     ! e6               ^
                                                                                                 end
  // -----------------------------------------------------------------------------------------------
  // tests
  // -----------------------------------------------------------------------------------------------

  def e1 = jobs groupBy month_of_submission must beAnInstanceOf[Map[LocalDate,Job]]
  def e2 = jobs groupBy project             must beAnInstanceOf[Map[String,Job]]
  def e3 = jobs groupBy department          must beAnInstanceOf[Map[String,Job]]
  def e4 = jobs groupBy SeqVsPar            must beAnInstanceOf[Map[String,Job]]
  def e5 = jobs groupBy group               must beAnInstanceOf[Map[String,Job]]
  def e6 = jobs groupBy owner               must beAnInstanceOf[Map[String,Job]]

  // -----------------------------------------------------------------------------------------------
  // utility functions
  // -----------------------------------------------------------------------------------------------

  def jobs: Seq[Job] = Seq()

}
