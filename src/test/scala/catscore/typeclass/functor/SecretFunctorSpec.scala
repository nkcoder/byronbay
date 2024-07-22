package my.playground
package catscore.typeclass.functor

import cats.Functor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SecretFunctorSpec extends AnyFlatSpec with Matchers {
  behavior of "Secret Functor"
  it should "map over Secret" in {
    import Secret.given

    val secret = Functor[Secret].map(Secret[String]("hello, world"))(_.toUpperCase)
    secret.value shouldEqual "HELLO, WORLD"
  }

  behavior of "Option Functor"
  it should "map over Option" in {
    import Secret.given

    val option = optionFunctor.map(Some(10))(_ + 1)
    option shouldEqual Some(11)
  }

  behavior of "List Functor"
  it should "map over List" in {
    import Secret.given

    val list = listFunctor.map(List(1, 2, 3))(_ * 2)
    list shouldEqual List(2, 4, 6)
  }

  it should "map over List of Secrets" in {
    import Secret.given

    val list = listFunctor.map(List(Secret(1), Secret(2), Secret(3)))(_.value * 2)
    list shouldEqual List(2, 4, 6)
  }

  behavior of "Functor as"
  it should "map to the same" in {
    import Secret.given
    optionFunctor.as(Some(10), "hello") shouldEqual Some("hello")
    listFunctor.as(List(1, 2, 3), "hello") shouldEqual List("hello", "hello", "hello")
  }

}
