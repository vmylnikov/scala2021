package scala2021.vmylnikov.task06

import scala2021.vmylnikov.task06.Main.Sex.Sex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {

  object Sex extends Enumeration {
    type Sex = Value
    val Male, Female = Value
  }

  case class UserForm(name: String, age: Int, email: String, sex: Sex, height: Double)

  case class User(name: String, age: Int, email: String, sex: Sex, height: Double)

  def validateName(name: String): Either[String, String] = {
    if (name == null || name.isEmpty || !"[a-zA-Z]+".r.matches(name))
      Left("Name must be non-empty and contain only latin letters")
    else
      Right(name)
  }

  def validateAge(age: Int): Either[String, Int] = {
    if (age > 0 && age < 100) Right(age) else Left("Age must be > 0 and < 100")
  }

  val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  def validateEmail(email: String): Either[String, String] = email match {
    case null => Right(email)
    case e if e.isEmpty => Right(e)
    case e if emailRegex.findFirstMatchIn(e).isDefined => Right(e)
    case _ => Left("Invalid email")
  }

  def validateHeight(height: Double, sex: Sex): Either[String, Double] = sex match {
    case Sex.Male if !(height > 100) => Left("Height must be > 100")
    case _ => Right(height)
  }

  // Выдавать только первую ошибку
  def validateFailFast(userForm: UserForm): Either[String, User] = {
    for {
      name <- validateName(userForm.name)
      age <- validateAge(userForm.age)
      email <- validateEmail(userForm.email)
      height <- validateHeight(userForm.height, userForm.sex)
    } yield User(name, age, email, userForm.sex, height)
  }

  // Выводить все возможные ошибки списком
  def validateFailSlow(userForm: UserForm): Either[List[String], User] = {
    val results = List(validateName(userForm.name),
      validateAge(userForm.age),
      validateEmail(userForm.email),
      validateHeight(userForm.height, userForm.sex))

    results.partitionMap(identity) match {
      case (Nil, List(name: String, age: Int, email: String, height: Double)) =>
        Right(User(name, age, email, userForm.sex, height))
      case (lefts, _) => Left(lefts)
    }
  }

  // Выводить все возможные ошибки, и проводить валидацию каждого поля параллельно(in parallel)
  def validateParallel(userForm: UserForm): Either[List[String], User] = {
    val nameFuture = Future(validateName(userForm.name))
    val ageFuture = Future(validateAge(userForm.age))
    val emailFuture = Future(validateEmail(userForm.email))
    val heightFuture = Future(validateHeight(userForm.height, userForm.sex))

    val result = for {
      nameResult <- nameFuture
      ageResult <- ageFuture
      emailResult <- emailFuture
      heightResult <- heightFuture
    } yield List(nameResult, ageResult, emailResult, heightResult)

    val results = Await.result(result, Duration("10 seconds"))
    results.partitionMap(identity) match {
      case (Nil, List(name: String, age: Int, email: String, height: Double)) =>
        Right(User(name, age, email, userForm.sex, height))
      case (lefts, _) => Left(lefts)
    }
  }

  val testUserFormList = List(
    UserForm("", 110, "invalid.email", Sex.Male, 98),
    UserForm(null, -1, "invalid email", Sex.Male, 99),
    UserForm("Игорь", -100, "invalid email", Sex.Male, 172),
    UserForm("John", 120, "john@example.com", Sex.Male, 98),
    UserForm("Mark", 42, "mark@example.com", Sex.Male, 99),
    UserForm("Igor", 30, "igor@@example.com", Sex.Male, 180),
    UserForm("Samuel", 35, "samuel@example.com", Sex.Male, 175),
    UserForm("Jane", 125, "jane@@example.com", Sex.Female, 160),
    UserForm("Megan", 10, "megan@example.com", Sex.Female, 95)
  )

  testUserFormList.foreach { userForm =>
    println(s"Validating fail-fast $userForm")
    validateFailFast(userForm) match {
      case Left(error) => println(error)
      case _ =>
    }
  }
  println()

  testUserFormList.foreach { userForm =>
    println(s"Validating fail-slow $userForm")
    validateFailSlow(userForm) match {
      case Left(errors) => errors.foreach(println)
      case _ =>
    }
  }
  println()

  testUserFormList.foreach { userForm =>
    println(s"Validating parallel $userForm")
    validateParallel(userForm) match {
      case Left(errors) => errors.foreach(println)
      case _ =>
    }
  }

}
