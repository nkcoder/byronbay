package my.playground
package catscore.eq

import org.scalatest.flatspec.AnyFlatSpec

class AccountSpec extends AnyFlatSpec {

  "Account" should "be equal by id" in {
    import cats.syntax.eq.given
    import Account.Instances.byIdEq2
    import cats.instances.long.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(1, "456", 200.0, "Jane")

    assert(a1 === a2)
  }

  it should "not be equal by id" in {
    import cats.syntax.eq.given
    import Account.Instances.byIdEq2
    import cats.instances.long.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "456", 200.0, "Jane")

    assert(a1 =!= a2)
  }

  it should "be equal by number" in {
    import cats.syntax.eq.given
    import Account.Instances.byNumberEq2
    import cats.instances.string.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "123", 200.0, "Jane")

    assert(a1 === a2)
  }

  it should "not be equal by number" in {
    import cats.syntax.eq.given
    import Account.Instances.byNumberEq2
    import cats.instances.string.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "456", 200.0, "Jane")

    assert(a1 =!= a2)
  }

  it should "not be equal by default universal Eq" in {
    import cats.syntax.eq.given

    val a1 = Account(1, "123", 100.0, "John")
    val a2 = Account(2, "456", 200.0, "Jane")

    assert(a1 =!= a2)
  }

}
