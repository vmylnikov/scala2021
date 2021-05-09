package scala2021.vmylnikov.task07

import java.io.{File, FileInputStream}

import scala.util.{Failure, Success, Try}

object Main extends App {

  case class Connection(port: Int = 8080) {
    println(s"Created connection on port $port")

    def close(): Unit = println("Closed")

    def run(): Unit = println("Run")
  }

  def withResource[A, B](create: => A)(run: A => B)(close: A => Unit = { _: A => }): B = {
    val resource = Try {
      create
    } match {
      case Failure(exception) => throw exception
      case Success(value) => value
    }

    Try {
      run(resource)
    } match {
      case Failure(exception) =>
        Try(close(resource))
        throw exception
      case Success(value) =>
        close(resource)
        value
    }
  }

  // Usage examples
  // Connection: no exception
  Try(
    withResource(Connection(9000)) {
      conn => conn.run()
    } {
      conn => conn.close()
    }
  ) match {
    case Success(_) =>
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

  // FileInputStream: no exception
  Try(
    withResource(new FileInputStream("README.md")) {
      fis => {
        val bytes = fis.available()
        println(s"Count bytes are $bytes")
      }
    } {
      fis => fis.close()
    }
  ) match {
    case Success(_) =>
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

  // FileInputStream: exception occurred
  Try(
    withResource(new FileInputStream("non-existing-file")) {
      fis => {
        val bytes = fis.available()
        println(s"Count bytes are $bytes")
      }
    } {
      fis => fis.close()
    }
  ) match {
    case Success(_) =>
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

  // File: no exception
  Try(
    withResource(new File("README.md")) {
      file => file.exists()
    }()
  ) match {
    case Success(fileExists) => println(s"fileExists: $fileExists")
    case Failure(exception) => println(s"An error occurred: ${exception.getMessage}")
  }

}
