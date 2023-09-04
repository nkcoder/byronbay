package my.playground
package intro

object BuyCoffees:
  @main
  def buyCoffee(): Unit =
    val cafe = Cafe()
    cafe.buyCoffee(CreditCard())

class Cafe:
  def buyCoffee(cc: CreditCard): (Coffee, Charge) =
    val cup = Coffee()
    (cup, Charge(cc, cup.price))

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) =
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    val charge = charges.reduce((c1, c2) => c1.combine(c2))
    (coffees, charge)

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList

class CreditCard:
  def charge(amount: Double): Unit =
    println("Charging: " + amount)

case class Charge(cc: CreditCard, amount: Double):
  def combine(other: Charge): Charge =
    if other.cc == cc then
      Charge(cc, amount + other.amount)
    else
      throw new Exception("cannot combine charges with different credit cards")
class Coffee:
  val price: Double = 2.0

