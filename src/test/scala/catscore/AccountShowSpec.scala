package my.playground
package catscore

import org.scalatest.flatspec.AnyFlatSpecLike

class AccountShowSpec extends AnyFlatSpecLike {
  "Account" should "show the same as toString" in {
    val account = Account(100L, "123-4567", 2000, "John Doe")
    assert(Account.toStringShow.show(account) == account.toString)
  }

  it should "show the custom message" in {
    val account = Account(100L, "123-4567", 2000, "John Doe")
    assert(Account.ShowInstances.customShow.show(account) == "123-4567 - $JOHN DOE")
  }

  it should "show using the syntax" in {
    val account = Account(100L, "123-4567", 2000, "John Doe")
    import Account.ShowInstances.customShow
    import cats.implicits.given
    assert(account.show == "123-4567 - $JOHN DOE")
  }

}
