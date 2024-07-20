package my.playground
package catscore.eq

import cats.kernel.Eq
import org.scalatest.flatspec.AnyFlatSpec

class EqSpec extends AnyFlatSpec {
  "Eq" should "be derived for BigDecimal" in {

    val a = BigDecimal(100.0)
    val b = BigDecimal(100.00)

    import Eq.catsKernelInstancesForBigDecimal
    val isEqual = Eq[BigDecimal].eqv(a, b)
    assert(isEqual)
  }

  it should "be derived for List" in {
    val a = List(1, 2, 3)
    val b = List(1, 2, 3)

    import Eq.catsKernelEqForList
    val isEqual = Eq[List[Int]].eqv(a, b)
    assert(isEqual)
  }

}
