package scala2021.vmylnikov.task09

object Main extends App {

  sealed trait Currency

  case object USD extends Currency

  case object EUR extends Currency

  case object GBP extends Currency

  final case class Money(amount: BigDecimal, currency: Currency) {
    def +(other: Money)(implicit exchanger: Exchanger): Either[String, Money] = {
      exchanger.exchange(other.currency, currency).map(rate => Money(amount + (other.amount * rate), currency))
    }
  }

  implicit class IntOps(private val x: Int) {
    def apply(c: Currency): Money = Money(x, c)
  }

  class EitherMoney(val e: Either[String, Money])(implicit val exchanger: Exchanger) {
    def to(c: Currency): Either[String, Money] = e match {
      case Right(money) => exchanger.exchange(money.currency, c).map(rate => Money(money.amount * rate, c))
      case Left(error) => Left(error)
    }
  }

  implicit def eitherToEitherMoney(e: Either[String, Money]): EitherMoney = new EitherMoney(e)

  final case class Conversion(from: Currency, to: Currency)

  final case class Exchanger(rates: Map[Conversion, BigDecimal]) {
    def exchange(from: Currency, to: Currency): Either[String, BigDecimal] = {
      if (from == to)
        Right(1)
      else
        rates.get(Conversion(from, to)).toRight(s"Can't find conversion from $from to $to")
    }
  }

  implicit val exchanger: Exchanger = Exchanger(
    Map(
      Conversion(EUR, USD) -> 1.2,
      Conversion(USD, GBP) -> 0.7
    )
  )

  val result = 42(USD) + 35(EUR)
  val resultToPound = result to GBP
  val resultToEUR = result to EUR
  val result2 = 42(USD) + 35(GBP)

  assert(result == Right(Money(42 + 35 * 1.2, USD)))
  assert(resultToPound == Right(Money((42 + 35 * 1.2) * 0.7, GBP)))
  assert(resultToEUR == Left("Can't find conversion from USD to EUR"))
  assert(result2 == Left("Can't find conversion from GBP to USD"))

}
