package my.playground
package catscore.eq

import cats.Eq

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  given universalEq: Eq[Account] = Eq.fromUniversalEquals

  object Instances {
    given byIdEq(using eqLong: Eq[Long]): Eq[Account] = Eq.instance((a1, a2) => eqLong.eqv(a1.id, a2.id))
    // this version is much more readable
    given byIdEq2(using Eq[Long]): Eq[Account] = Eq.by(_.id)

    given byNumberEq(using eqString: Eq[String]): Eq[Account] =
      Eq.instance((a1, a2) => eqString.eqv(a1.number, a2.number))
    // prefer this version
    given byNumberEq2(using Eq[String]): Eq[Account] = Eq.by(_.number)
  }
  
  Eq.catsKernelEqForList
}
