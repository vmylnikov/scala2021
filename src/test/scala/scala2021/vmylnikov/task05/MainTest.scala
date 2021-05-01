package scala2021.vmylnikov.task05

import org.scalatest.funsuite.AnyFunSuite
import scala2021.vmylnikov.task05.Main.Info

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class MainTest extends AnyFunSuite {

  test("testFindManagerName") {
    assert(Main.findManagerName("John").isEmpty)
    assert(Main.findManagerName("Steve").contains("Steve"))
    assert(Main.findManagerName("Samuel").contains("Igor"))
    assert(Main.findManagerName("Mark").contains("Steve"))
    assert(Main.findManagerName("Igor").contains("Igor"))
    assert(Main.findManagerName("Christy").isEmpty)
    assert(Main.findManagerName("Naveen").isEmpty)
    assert(Main.findManagerName("Megan").isEmpty)
  }

  test("testFindManagerNameOrError") {
    assert(Main.findManagerNameOrError("John") == Left("Couldn't find employee by name John"))
    assert(Main.findManagerNameOrError("Steve") == Right("Steve"))
    assert(Main.findManagerNameOrError("Samuel") == Right("Igor"))
    assert(Main.findManagerNameOrError("Mark") == Right("Steve"))
    assert(Main.findManagerNameOrError("Igor") == Right("Igor"))
    assert(Main.findManagerNameOrError("Christy") == Left("Couldn't find department by id 5"))
    assert(Main.findManagerNameOrError("Naveen") == Left("Couldn't find employee by id 14"))
    assert(Main.findManagerNameOrError("Megan") == Left("Couldn't find manager by department name Research"))
  }

  test("testFindManagerNameOrErrorAsync") {
    val result = for {
      r1 <- Main.findManagerNameOrErrorAsync("John")
      r2 <- Main.findManagerNameOrErrorAsync("Steve")
      r3 <- Main.findManagerNameOrErrorAsync("Samuel")
      r4 <- Main.findManagerNameOrErrorAsync("Mark")
      r5 <- Main.findManagerNameOrErrorAsync("Igor")
      r6 <- Main.findManagerNameOrErrorAsync("Christy")
      r7 <- Main.findManagerNameOrErrorAsync("Naveen")
      r8 <- Main.findManagerNameOrErrorAsync("Megan")
    } yield (r1, r2, r3, r4, r5, r6, r7, r8)

    val completed = Await.result(result, Duration("10 seconds"))
    assert(completed._1 == Left("Couldn't find employee by name John"))
    assert(completed._2 == Right("Steve"))
    assert(completed._3 == Right("Igor"))
    assert(completed._4 == Right("Steve"))
    assert(completed._5 == Right("Igor"))
    assert(completed._6 == Left("Couldn't find department by id 5"))
    assert(completed._7 == Left("Couldn't find employee by id 14"))
    assert(completed._8 == Left("Couldn't find manager by department name Research"))
  }

  test("testFindEmployeeManagers") {
    val expected = List(
      Info("Steve", "Marketing", "Steve"),
      Info("Mark", "Marketing", "Steve"),
      Info("Jane", "Marketing", "Steve"),
      Info("Samuel", "Sales", "Igor"),
      Info("Igor", "Sales", "Igor"),
      Info("Naveen", "IT", "Not Found"),
      Info("Christy", "Not Found", "Not Found"),
      Info("Megan", "Research", "Not Found"))
    assert(Main.findEmployeeManagers == expected)
  }

}
