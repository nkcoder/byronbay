package my.playground
package catscore

import org.scalatest.flatspec.AnyFlatSpec

class AccountEqSpec extends AnyFlatSpec {
  behavior of "Account Eq"

  it should "be equal by id" in {
    import cats.syntax.eq.given
    import Account.EqInstances.eqById2
    import cats.instances.long.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(1, "456", 200.0, "Jane")

    assert(a1 === a2)
  }

  it should "not be equal by id" in {
    import cats.syntax.eq.given
    import Account.EqInstances.eqById2
    import cats.instances.long.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "456", 200.0, "Jane")

    assert(a1 =!= a2)
  }

  it should "be equal by number" in {
    import cats.syntax.eq.given
    import Account.EqInstances.eqByNumber2
    import cats.instances.string.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "123", 200.0, "Jane")

    assert(a1 === a2)
  }

  it should "not be equal by number" in {
    import cats.syntax.eq.given
    import Account.EqInstances.eqByNumber2
    import cats.instances.string.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "456", 200.0, "Jane")

    assert(a1 =!= a2)
  }

  it should "not be equal by default universal Eq" in {
    import cats.syntax.eq.given
    import Account.universalEq

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "456", 200.0, "Jane")

    assert(a1 =!= a2)
  }

  "Eq" should "be derived for BigDecimal" in {
    import cats.kernel.Eq

    val a = BigDecimal(100.0)
    val b = BigDecimal(100.00)

    import Eq.catsKernelInstancesForBigDecimal
    val isEqual = Eq[BigDecimal].eqv(a, b)
    assert(isEqual)
  }

  it should "be derived for List" in {
    import cats.kernel.Eq

    val a = List(1, 2, 3)
    val b = List(1, 2, 3)

    import Eq.catsKernelEqForList
    val isEqual = Eq[List[Int]].eqv(a, b)
    assert(isEqual)
  }

}
