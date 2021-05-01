package scala2021.vmylnikov.task05

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

  // helper functions
  def findDepartmentByEmployee(employee: Option[Employee]): Option[Department] = employee match {
    case Some(employee) => departments.find(_.id == employee.departmentId)
    case None => None
  }

  def findManagerByDepartment(department: Option[Department]): Option[Manager] = department match {
    case Some(department) => managers.find(_.department == department.name)
    case None => None
  }

  def findEmployeeByManager(manager: Option[Manager]): Option[Employee] = manager match {
    case Some(manager) => employees.find(_.id == manager.employeeId)
    case None => None
  }

  // Найти имя менеджера департамента, в котором работает сотрудник по имени сотрудника
  def findManagerName(employee: String): Option[String] = {
    for {
      employeeObj <- employees.find(_.name == employee)
      department <- findDepartmentByEmployee(Option(employeeObj))
      manager <- findManagerByDepartment(Option(department))
      managerEmployee <- findEmployeeByManager(Option(manager))
    } yield managerEmployee.name
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так
  def findManagerNameOrError(employee: String): Either[String, String] = {
    for {
      employeeObj <- employees.find(_.name == employee)
        .toRight(s"Couldn't find employee by name $employee")
      department <- findDepartmentByEmployee(Option(employeeObj))
        .toRight(s"Couldn't find department by id ${employeeObj.departmentId}")
      manager <- findManagerByDepartment(Option(department))
        .toRight(s"Couldn't find manager by department name ${department.name}")
      managerEmployee <- findEmployeeByManager(Option(manager))
        .toRight(s"Couldn't find employee by id ${manager.employeeId}")
    } yield managerEmployee.name
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать все это асинхронно
  def findManagerNameOrErrorAsync(employee: String): Future[Either[String, String]] = Future {
    findManagerNameOrError(employee)
  }

  // вывести список всех сотрудников, вместе с именем департамента и именем менеджера, если департамента или менеджера нет то использовать константу "Not Found"
  def findEmployeeManagers: List[Info] = {
    employees
      .map(e => (e.name, findDepartmentByEmployee(Option(e))))
      .map { case (e, d) => (e, d, findManagerByDepartment(d)) }
      .map { case (e, d, m) =>
        Info(e, d.map(_.name).getOrElse(NotFound), findEmployeeByManager(m).map(_.name).getOrElse(NotFound))
      }
  }

}
