package scala2021.vmylnikov.task03


import scala.annotation.tailrec

object Main extends App {

  def encodeDirect(chars: List[Char]): List[(Int, Char)] = {
    @tailrec
    def loop(acc: List[(Int, Char)], list: List[Char]): List[(Int, Char)] = list match {
      case Nil => acc
      case _ =>
        val (leftList, rightList) = list.span(_ == list.head)
        loop(acc ::: List((leftList.length, leftList.head)), rightList)
    }

    loop(List.empty, chars)
  }

  println(encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
}
