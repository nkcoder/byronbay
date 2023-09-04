package my.playground
package getstarted

import MyProgram.{factorial, fibonacci}
import org.scalatest.funsuite.AnyFunSuite

class MyProgramSpec extends AnyFunSuite:
  test(testName = "factorial should handle n > 0") {
    assert(factorial(4) == 24)
  }

  test(testName = "factorial should handle n == 0") {
    assert(factorial(0) == 1)
  }

  test(testName = "fibonacci should handle n == 1") {
    assert(fibonacci(1) == 0)
  }

  test(testName = "fibonacci should handle n == 2") {
    assert(fibonacci(2) == 1)
  }

  test(testName = "fibonacci should handle n > 2") {
    assert(fibonacci(6) == 5)
  }
