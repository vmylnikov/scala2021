package scala2021.vmylnikov.task05

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class MainSuit extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {

  import scala2021.vmylnikov.task05.Main._

  private val tblFindManagerNameData = Table(
    ("employee", "expected"),
    ("John", None),
    ("Steve", Some("Steve")),
    ("Samuel", Some("Igor")),
    ("Mark", Some("Steve")),
    ("Igor", Some("Igor")),
    ("Christy", None),
    ("Naveen", None),
    ("Megan", None)
  )

  test("check findManagerName returns correct result (table)") {
    forAll(tblFindManagerNameData) {
      (employee, expected) => {
        findManagerName(employee) should be(expected)
      }
    }
  }

  private val tblFindManagerNameOrErrorData = Table(
    ("employee", "expected"),
    ("John", Left("Couldn't find employee by name John")),
    ("Steve", Right("Steve")),
    ("Samuel", Right("Igor")),
    ("Mark", Right("Steve")),
    ("Igor", Right("Igor")),
    ("Christy", Left("Couldn't find department by id 5")),
    ("Naveen", Left("Couldn't find employee by id 14")),
    ("Megan", Left("Couldn't find manager by department name Research"))
  )

  test("check findManagerNameOrError returns correct result (table)") {
    forAll(tblFindManagerNameOrErrorData) {
      (employee, expected) => {
        findManagerNameOrError(employee) should be(expected)
      }
    }
  }

  test("check findEmployeeManagers returns correct result") {
    val expected = List(
      Info("Steve", "Marketing", "Steve"),
      Info("Mark", "Marketing", "Steve"),
      Info("Jane", "Marketing", "Steve"),
      Info("Samuel", "Sales", "Igor"),
      Info("Igor", "Sales", "Igor"),
      Info("Naveen", "IT", "Not Found"),
      Info("Christy", "Not Found", "Not Found"),
      Info("Megan", "Research", "Not Found"))
    findEmployeeManagers should be(expected)
  }

}
