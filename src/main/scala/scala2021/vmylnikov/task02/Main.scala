package scala2021.vmylnikov.task02

import scala.annotation.tailrec

object Main extends App {

  def balanced(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars: List[Char], openCount: Int): Boolean = {
      if (chars.isEmpty) openCount == 0
      else if (chars.head == '(') loop(chars.tail, openCount + 1)
      else if (chars.head == ')') openCount > 0 && loop(chars.tail, openCount - 1)
      else loop(chars.tail, openCount)
    }

    loop(chars, 0)
  }

  println(balanced("if((2+x)*(3-y)==3)".toList))
  println(balanced("Я сказал ему (это еще (не) сделано). (Но он не послушал)".toList))
  println(balanced(":-)".toList))
  println(balanced("())(".toList))
}
