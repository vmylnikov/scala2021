package scala2021.vmylnikov.task02

import scala.annotation.tailrec

object Main extends App {

  def balanced(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars: List[Char], openCount: Int): Boolean = chars match {
      case Nil => openCount == 0
      case _ => chars.head match {
        case '(' => loop(chars.tail, openCount + 1)
        case ')' => openCount > 0 && loop(chars.tail, openCount - 1)
        case _ => loop(chars.tail, openCount)
      }
    }

    loop(chars, 0)
  }

  println(balanced("if((2+x)*(3-y)==3)".toList))
  println(balanced("Я сказал ему (это еще (не) сделано). (Но он не послушал)".toList))
  println(balanced(":-)".toList))
  println(balanced("())(".toList))
}
