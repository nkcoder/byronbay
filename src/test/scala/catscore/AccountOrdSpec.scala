package my.playground
package catscore

import cats.Order
import org.scalatest.flatspec.AnyFlatSpec

class AccountOrdSpec extends AnyFlatSpec {
  def sort[A](list: List[A])(using orderA: Order[A]): List[A] =
    list.sorted(orderA.toOrdering)

  "AccountOrd" should "order accounts by id" in {
    val a = Account(2, "123", 100.0, "John")
    val b = Account(1, "456", 105.0, "Kate")

    val list = List(a, b)
    sort(list) == List(b, a)
  }

  it should "order accounts by id DESC" in {
    given orderByIdDesc: Order[Account] = Order.reverse(Account.orderById)

    val a = Account(2, "123", 100.0, "John")
    val b = Account(1, "456", 105.0, "Kate")

    sort(List(a, b)) == List(a, b)
  }

  it should "order accounts by number" in {
    import Account.OrderInstances.orderByNumber

    val a = Account(2, "123", 100.0, "John")
    val b = Account(1, "456", 105.0, "Kate")

    val list = List(a, b)
    sort(list) == List(a, b)
  }

  it should "order accounts by balance" in {
    import Account.OrderInstances.orderByBalance

    val a = Account(2, "123", 200.0, "John")
    val b = Account(1, "456", 105.0, "Kate")

    val list = List(a, b)
    sort(list) == List(b, a)
  }

  it should "compare accounts by balance" in {
    import cats.syntax.order.given
    import Account.OrderInstances.orderByBalance

    val a = Account(2, "123", 200.0, "John")
    val b = Account(1, "456", 105.0, "Kate")

    assert((a compare b) == 1)
    assert((a max b) == a)
  }
}
