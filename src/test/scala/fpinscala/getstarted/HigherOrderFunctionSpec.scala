package my.playground
package fpinscala.getstarted

import org.scalatest.funsuite.AnyFunSuite
import HigherOrderFunction.*

class HigherOrderFunctionSpec extends AnyFunSuite {
  test(testName = "findFirst should find the element when it exists") {
    val index = findFirst(Array(1, 3, 5, 8, 9), x => x % 2 == 0)
    assert(index == 3)
  }

  test(testName = "findFirst should NOT find the element when it does not exist") {
    val index = findFirst(Array(1, 3, 5, 7, 9), x => x % 2 == 0)
    assert(index == -1)
  }

  test(testName = "isSorted should handle sorted data") {
    val sorted = isSorted(Array(1, 3, 8, 10, 40), (x, y) => x < y)
    assert(sorted)
  }

  test(testName = "isSorted should handle unsorted data") {
    val sorted = isSorted(Array(1, 3, 8, 10, 40, 9, 7), (x, y) => x < y)
    assert(!sorted)
  }
}
