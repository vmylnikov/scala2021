package scala2021.vmylnikov.task07

import java.io.{File, FileInputStream}

import scala.util.{Failure, Success, Try}

object Main extends App {

  case class Connection(port: Int = 8080) {
    println(s"Created connection on port $port")

    def close(): Unit = println("Closed")

    def run(): Unit = println("Run")
  }

  def withResource[T, R](create: => T)(run: T => R)(close: T => Unit = { _: T => }): Try[R] = {
    Try(create).flatMap(t =>
      Try(run(t)) match {
        case Success(value) => Try(close(t)).map(_ => value)
        case failure => Try(close(t)); failure
      }
    )
  }

  // Usage examples
  // Connection: no exception
  withResource(Connection(9000)) {
    conn => conn.run()
  } {
    conn => conn.close()
  } match {
    case Success(_) =>
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

  // FileInputStream: no exception
  withResource(new FileInputStream("README.md")) {
    fis => {
      val bytes = fis.available()
      println(s"Count bytes are $bytes")
    }
  } {
    fis => fis.close()
  } match {
    case Success(_) =>
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

  // FileInputStream: exception occurred
  withResource(new FileInputStream("non-existing-file")) {
    fis => {
      val bytes = fis.available()
      println(s"Count bytes are $bytes")
    }
  } {
    fis => fis.close()
  } match {
    case Success(_) =>
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

  // File: no exception
  withResource(new File("README.md")) {
    file => file.exists()
  }() match {
    case Success(fileExists) => println(s"fileExists: $fileExists")
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

}
