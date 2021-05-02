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

  case class Person(name: String, age: Int, email: String, sex: Sex, height: Double)

  def validateName(name: String): Either[String, Unit] = {
    def isEmpty(x: String) = x == null || x.isEmpty

    def containsOnlyLatin(s: String): Boolean = s.toList.forall(c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))

    if (isEmpty(name) || !containsOnlyLatin(name))
      Left("Name must be non-empty and contain only latin letters")
    else
      Right()
  }

  def validateAge(age: Int): Either[String, Unit] = {
    if (age > 0 && age < 100) Right() else Left("Age must be > 0 and < 100")
  }

  val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  def validateEmail(email: String): Either[String, Unit] = email match {
    case null => Right()
    case e if e.isEmpty => Right()
    case e if emailRegex.findFirstMatchIn(e).isDefined => Right()
    case _ => Left("Invalid email")
  }

  def validateHeight(height: Double, sex: Sex): Either[String, Unit] = sex match {
    case Sex.Male if !(height > 100) => Left("Height must be > 100")
    case _ => Right()
  }

  // Выдавать только первую ошибку
  def validateFailFast(person: Person): List[String] = {
    val result = for {
      _ <- validateName(person.name)
      _ <- validateAge(person.age)
      _ <- validateEmail(person.email)
      _ <- validateHeight(person.height, person.sex)
    } yield ()

    result match {
      case Left(error) => List(error)
      case _ => Nil
    }
  }

  // Выводить все возможные ошибки списком
  def validateFailSlow(person: Person): List[String] = {
    val (left, _) = List(
      validateName(person.name),
      validateAge(person.age),
      validateEmail(person.email),
      validateHeight(person.height, person.sex)
    ).partition(_.isLeft)

    left.collect { case Left(error) => error }
  }

  // Выводить все возможные ошибки, и проводить валидацию каждого поля параллельно(in parallel)
  def validateParallel(person: Person): List[String] = {
    val result = for {
      r1 <- Future(validateName(person.name))
      r2 <- Future(validateAge(person.age))
      r3 <- Future(validateEmail(person.email))
      r4 <- Future(validateHeight(person.height, person.sex))
    } yield List(r1, r2, r3, r4)

    val (left, _) = Await.result(result, Duration("10 seconds")).partition(_.isLeft)
    left.collect { case Left(error) => error }
  }

  val personList = List(
    Person("", 110, "invalid.email", Sex.Male, 98),
    Person(null, -1, "invalid email", Sex.Male, 99),
    Person("Игорь", -100, "invalid email", Sex.Male, 172),
    Person("John", 120, "john@example.com", Sex.Male, 98),
    Person("Mark", 42, "mark@example.com", Sex.Male, 99),
    Person("Igor", 30, "igor@@example.com", Sex.Male, 180),
    Person("Samuel", 35, "samuel@example.com", Sex.Male, 175),
    Person("Jane", 125, "jane@@example.com", Sex.Female, 160),
    Person("Megan", 10, "megan@example.com", Sex.Female, 95)
  )

  personList.foreach { person =>
    println(s"Validating fail-fast $person")
    validateFailFast(person).foreach(println)
  }
  println()

  personList.foreach { person =>
    println(s"Validating fail-slow $person")
    validateFailSlow(person).foreach(println)
  }
  println()

  personList.foreach { person =>
    println(s"Validating parallel $person")
    validateParallel(person).foreach(println)
  }
  println()

}
