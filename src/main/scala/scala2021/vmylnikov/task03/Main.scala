package scala2021.vmylnikov.task03

object Main extends App {

  def encodeDirect(chars: List[Char]): List[(Int, Char)] = {
    chars.foldLeft(List.empty[(Int, Char)]) { (acc, e) =>
      acc match {
        case Nil => (1, e) :: Nil
        case (lastCharCount, lastChar) :: xs if lastChar == e => (lastCharCount + 1, lastChar) :: xs
        case xs => (1, e) :: xs
      }
    }.reverse
  }

  println(encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
}
