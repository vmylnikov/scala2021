package scala2021.vmylnikov.task05

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main {

  case class Employee(id: Int, name: String, departmentId: Int)

  case class Department(id: Int, name: String)

  case class Manager(department: String, employeeId: Int)

  case class Info(employee: String, department: String, manager: String)

  val employees = List(
    Employee(1, "Steve", 1),
    Employee(3, "Mark", 1),
    Employee(4, "Jane", 1),
    Employee(7, "Samuel", 2),
    Employee(10, "Igor", 2),
    Employee(11, "Naveen", 4),
    Employee(12, "Christy", 5),
    Employee(15, "Megan", 3)
  )

  val departments = List(
    Department(1, "Marketing"),
    Department(2, "Sales"),
    Department(3, "Research"),
    Department(4, "IT"),
  )

  val managers = List(
    Manager("Marketing", 1),
    Manager("Sales", 10),
    Manager("IT", 14),
  )

  val NotFound = "Not Found"

  // Найти имя менеджера департамента, в котором работает сотрудник по имени сотрудника
  def findManagerName(employee: String): Option[String] = {
    for {
      employeeObj <- employees.find(_.name == employee)
      department <- departments.find(_.id == employeeObj.departmentId)
      manager <- managers.find(_.department == department.name)
      managerEmployee <- employees.find(_.id == manager.employeeId)
    } yield managerEmployee.name
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так
  def findManagerNameOrError(employee: String): Either[String, String] = {
    for {
      employeeObj <- employees.find(_.name == employee)
        .toRight(s"Couldn't find employee by name $employee")
      department <- departments.find(_.id == employeeObj.departmentId)
        .toRight(s"Couldn't find department by id ${employeeObj.departmentId}")
      manager <- managers.find(_.department == department.name)
        .toRight(s"Couldn't find manager by department name ${department.name}")
      managerEmployee <- employees.find(_.id == manager.employeeId)
        .toRight(s"Couldn't find employee by id ${manager.employeeId}")
    } yield managerEmployee.name
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать все это асинхронно
  def findManagerNameOrErrorAsync(employee: String): Future[Either[String, String]] = Future {
    findManagerNameOrError(employee)
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать каждую операцию асинхронной(операция = вызов репозитория)
  def findManagerNameOrErrorAsyncOperations(employee: String): Future[Either[String, String]] = Future {
    val result = for {
      employeeEither <- Future {
        employees.find(_.name == employee).toRight(s"Couldn't find employee by name $employee")
      }
      departmentEither <- Future {
        employeeEither.map { employeeObj =>
          departments.find(_.id == employeeObj.departmentId)
            .toRight(s"Couldn't find department by id ${employeeObj.departmentId}")
        }.flatten
      }
      managerEither <- Future {
        departmentEither.map { department =>
          managers.find(_.department == department.name)
            .toRight(s"Couldn't find manager by department name ${department.name}")
        }.flatten
      }
      managerEmployeeNameEither <- Future {
        managerEither.map { manager =>
          employees.find(_.id == manager.employeeId).map(_.name)
            .toRight(s"Couldn't find employee by id ${manager.employeeId}")
        }.flatten
      }
    } yield managerEmployeeNameEither

    Await.result(result, Duration("10 seconds"))
  }

  // вывести список всех сотрудников, вместе с именем департамента и именем менеджера, если департамента или менеджера нет то использовать константу "Not Found"
  def findEmployeeManagers: List[Info] = {
    def createInfo(e: Employee) = for {
      d <- departments.find(_.id == e.departmentId).toRight(Info(e.name, NotFound, NotFound))
      m <- managers.find(_.department == d.name).toRight(Info(e.name, d.name, NotFound))
      me <- employees.find(_.id == m.employeeId).toRight(Info(e.name, d.name, NotFound))
    } yield Info(e.name, d.name, me.name)

    employees.map(e => createInfo(e).merge)
  }

}
