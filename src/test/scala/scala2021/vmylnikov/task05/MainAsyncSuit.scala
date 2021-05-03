package scala2021.vmylnikov.task05

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import scala2021.vmylnikov.task05.Main.{findManagerNameOrErrorAsync, findManagerNameOrErrorAsyncOperations}

class MainAsyncSuit extends AsyncFunSuite with TableDrivenPropertyChecks with Matchers {

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

  test("check findManagerNameOrErrorAsync returns correct result (table)") {
    forAll(tblFindManagerNameOrErrorData) {
      (employee, expected) => {
        findManagerNameOrErrorAsync(employee).map {
          _ should be(expected)
        }
      }
    }
  }

  test("check findManagerNameOrErrorAsyncOperations returns correct result (table)") {
    forAll(tblFindManagerNameOrErrorData) {
      (employee, expected) => {
        findManagerNameOrErrorAsyncOperations(employee).map {
          _ should be(expected)
        }
      }
    }
  }

}
