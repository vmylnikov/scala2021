package scala2021.vmylnikov.task04

import scala.annotation.tailrec

object Main extends App {

  // without @tailrec
  def canChange(money: Int, coins: List[Int]): Boolean = {
    assert(money > 0, "Money must be > 0")

    def canChangeRecurse(money: Int, coins: List[Int]): Boolean = money match {
      case 0 => true
      case x if x > 0 && coins.nonEmpty => canChangeRecurse(x - coins.head, coins) || canChangeRecurse(x, coins.tail)
      case _ => false
    }

    canChangeRecurse(money, coins)
  }

  // with @tailrec
  def canChangeWithTailrec(money: Int, coins: List[Int]): Boolean = {
    assert(money > 0, "Money must be > 0")

    @tailrec
    def canChangeRecurse(possibleSums: List[Int]): Boolean = {
      possibleSums.distinct.filter(x => x <= money) match {
        case Nil => false
        case xs if xs.contains(money) => true
        case xs => canChangeRecurse(xs.flatMap(x => coins.map(coin => x + coin)))
      }
    }

    canChangeRecurse(coins)
  }

  assert(canChange(8, List(2, 4, 6)))
  assert(!canChange(5, List(2, 4, 6)))
  assert(canChange(17, List(3, 2)))
  assert(canChange(1000, List(31, 7)))

  assert(canChangeWithTailrec(8, List(2, 4, 6)))
  assert(!canChangeWithTailrec(5, List(2, 4, 6)))
  assert(canChangeWithTailrec(17, List(3, 2)))
  assert(canChangeWithTailrec(1000, List(31, 7)))

}
