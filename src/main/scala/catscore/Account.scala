package my.playground
package catscore

import cats.Eq
import cats.kernel.Order

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {

  /**
   * Eq
   */
  given universalEq: Eq[Account] = Eq.fromUniversalEquals

  object EqInstances {
    given eqById(using eqLong: Eq[Long]): Eq[Account] = Eq.instance((a1, a2) => eqLong.eqv(a1.id, a2.id))

    // this version is much more readable
    given eqById2(using Eq[Long]): Eq[Account] = Eq.by(_.id)

    given eqByNumber(using eqString: Eq[String]): Eq[Account] =
      Eq.instance((a1, a2) => eqString.eqv(a1.number, a2.number))

    // prefer this version
    given eqByNumber2(using Eq[String]): Eq[Account] = Eq.by(_.number)
  }

  /**
   * Ord
   */
  // use Order.by
  given orderById(using Order[Long]): Order[Account] = Order.by(_.id)

  object OrderInstances {
    // use Order.from
    given orderById2(using orderLong: Order[Long]): Order[Account] = Order.from((x, y) => orderLong.compare(x.id, y.id))

    given orderByNumber(using Order[String]): Order[Account] = Order.by(_.number)

    given orderByBalance(using Order[Double]): Order[Account] = Order.by(_.balance)
  }

}
