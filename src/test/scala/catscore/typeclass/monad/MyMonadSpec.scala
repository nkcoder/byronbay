package my.playground
package catscore.typeclass.monad

import scala.util.{Failure, Success}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyMonadSpec extends AnyFlatSpec with Matchers {
  behavior of "MyMonad -> optionMonad"
  it should "pure" in {
    import MyMonad.given
    optionMonad.pure(1) shouldEqual Some(1)
  }
  it should "flatMap" in {
    import MyMonad.given
    optionMonad.flatMap(Some(1))(a => Some(a + 1)) shouldEqual Some(2)
    optionMonad.flatMap[Int, Int](None)(a => Some(a + 1)) shouldEqual None
  }
  it should "map" in {
    import MyMonad.given
    optionMonad.map(Some(1))(a => a + 1) shouldEqual Some(2)
  }
  it should "flatten" in {
    import MyMonad.given
    optionMonad.flatten(Some(Some(1))) shouldEqual Some(1)
    optionMonad.flatten(None) shouldEqual None
  }
  it should "for-comprehension" in {
    import MyMonad.given
    val result = for {
      a <- optionMonad.pure(3)
      b <- optionMonad.pure(4)
    } yield a + b
    result shouldEqual Some(7)
  }

  behavior of "MyMonad -> tryMonad"
  it should "pure" in {
    import MyMonad.given
    tryMonad.pure(1) shouldEqual Success(1)
  }
  it should "flatMap" in {
    import MyMonad.given
    tryMonad.flatMap(Success(1))(a => Success(a + 1)) shouldEqual Success(2)
    tryMonad.flatMap[Int, Int](Success(1))(a => Success(a + 1)) shouldEqual Success(2)
    val exception = new Exception("error")
    tryMonad.flatMap[Int, Int](Success(1))(a => Failure(exception)) shouldEqual Failure(exception)
  }
  it should "map" in {
    import MyMonad.given
    tryMonad.map(Success(1))(a => a + 1) shouldEqual Success(2)
  }
  it should "flatten" in {
    import MyMonad.given
    tryMonad.flatten(Success(Success(1))) shouldEqual Success(1)
    val exception = new Exception("error")
    tryMonad.flatten(Failure(exception)) shouldEqual Failure(exception)
  }

}
