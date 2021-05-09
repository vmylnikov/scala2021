package scala2021.vmylnikov.task09

object Main extends App {

  sealed trait Currency

  case object USD extends Currency

  case object EUR extends Currency

  case object GBP extends Currency

  final case class Money(amount: BigDecimal, currency: Currency) {
    def +(other: Money)(implicit exchanger: Exchanger): Money = {
      val rate: BigDecimal = exchanger.exchange(other.currency, currency)

      Money(amount + (other.amount * rate), currency)
    }

    def to(otherCurrency: Currency)(implicit exchanger: Exchanger): Money = {
      val rate: BigDecimal = exchanger.exchange(currency, otherCurrency)

      Money(amount * rate, otherCurrency)
    }
  }

  implicit class IntOps(private val x: Int) {
    def apply(c: Currency): Money = Money(x, c)
  }

  final case class Conversion(from: Currency, to: Currency)

  final case class Exchanger(rates: Map[Conversion, BigDecimal]) {
    def exchange(from: Currency, to: Currency): BigDecimal = if (from == to) 1 else rates(Conversion(from, to))
  }

  implicit val exchanger: Exchanger = Exchanger(
    Map(
      Conversion(EUR, USD) -> 1.2,
      Conversion(USD, GBP) -> 0.7
    )
  )

  val result = 42(USD) + 35(EUR)
  val resultToPound = result to GBP

  assert(result == Money(42 + 35 * 1.2, USD))
  assert(resultToPound == Money((42 + 35 * 1.2) * 0.7, GBP))

}
