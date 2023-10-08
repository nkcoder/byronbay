package my.playground
package fpinscala.exceptions

import fpinscala.exceptions.Either.{Left, Right, safeDiv}

import org.scalatest.funsuite.AnyFunSuite

class EitherSpec extends AnyFunSuite:
  test(testName = "safeDiv: should return Left when divide by zero") {
    assert(safeDiv(10, 0).isLeft)
  }

  test(testName = "safeDiv: should return right when divide gets a result") {
    assert(safeDiv(10, 2).isRight)
    assert(safeDiv(10, 2) == Right(5))
  }

  test(testName = "Person: should return Left with error message about name") {
    assert(Person.make("", 10) == Left("Name is empty"))
  }

  test(testName = "Person: should return Left with error message about age") {
    assert(Person.make("Kate", -10) == Left("Age is out of range"))
  }

  test(testName = "Person: should create person") {
    val p = Person.make("Kate", 10)
    assert(p.isRight)
  }

  test(testName = "Person: should report all errors") {
    val p = Person.makeBoth("", -5)
    assert(p == Left(List("Name is empty", "Age is out of range")))
  }
end EitherSpec
