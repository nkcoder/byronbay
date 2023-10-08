package my.playground
package fpinscala.exceptions

import fpinscala.exceptions.Option.{None, Some, abs0, map2, parseInt, sequence, sequence2}

import org.scalatest.funsuite.AnyFunSuite

class OptionSpec extends AnyFunSuite:
  test(testName = "abs0: should lift abs to Option context") {
    assert(abs0(None) == None)
    assert(abs0(Some(-50)) == Some(50))
    assert(abs0(Some(20)) == Some(20))
  }

  test(testName = "map2: should combine two Option values using a binary function") {
    // use a separate parameter
    val multiplyOpt = map2(Some(2), Some(3))((x, y) => x * y)
    assert(multiplyOpt == Some(6))

    // use a brace delimited block
    // val multiplyOpt2 = map2(Some(2), Some(3)){(x, y) => x * y}

    // use an indented block
    val divideOpt = map2(Some(20), Some(5)): (x, y) =>
      x / y
    assert(divideOpt == Some(4))

    val sumOpt = map2(Some(5), Some(10))(_ + _)
    assert(sumOpt == Some(15))

    val minusOpt = map2(Some(3), None: Option[Int])((x, y) => x - y)
    assert(minusOpt == None)
  }

  test(testName = "sequence: combine List[Option[A]] to Option[List[A]]") {
    assert(sequence(List(Some(4), Some(4), Some(5), Some(3))) == Some(List(4, 4, 5, 3)))
    assert(sequence2(List(Some(4), Some(4), Some(5), Some(3))) == Some(List(4, 4, 5, 3)))

    assert(sequence(List(Some(4), Some(4), Some(5), None)) == None)
    assert(sequence2(List(Some(4), Some(4), Some(5), None)) == None)

    val parseResult = parseInt(List("22", "11", "33"))
    assert(parseResult == Some(List(22, 11, 33)))
    val parseResult2 = parseInt(List("hello", "44", "55"))
    assert(parseResult2 == None)
  }
