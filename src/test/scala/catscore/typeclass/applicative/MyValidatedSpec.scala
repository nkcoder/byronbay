package my.playground
package catscore.typeclass.applicative

import cats.Applicative
import cats.data.NonEmptyChain
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyValidatedSpec extends AnyFlatSpec with Matchers {
  behavior of "Applicative => MyValidated"
  it should "ap" in {
    val validated = Applicative[MyValidated].ap(MyValidated.Valid((a: Int) => a + 1))(MyValidated.Valid(10))
    validated shouldEqual MyValidated.Valid(11)
  }

  it should "map2" in {

    val validated = Applicative[MyValidated].map2(MyValidated.Valid(10), MyValidated.Valid(20))(_ + _)
    validated shouldEqual MyValidated.Valid(30)
  }

  it should "mapN" in {
    val v1 = Applicative[MyValidated].pure(10)
    val v2 = Applicative[MyValidated].pure(20)
    val v3 = Applicative[MyValidated].pure(30)
    (v1, v2, v3).mapN(_ + _ + _) shouldEqual MyValidated.Valid(60)
  }

  behavior of "Applicative => Validated"
  it should "mapN" in {
    import cats.data.Validated
    import cats.data.Validated.Valid

    val v1 = Validated.valid[String, Int](10)
    val v2 = Validated.valid[String, Int](20)
    val v3 = Validated.valid[String, Int](30)

    (v1, v2, v3).mapN(_ * _ * _) shouldEqual Valid(6000)
  }

  it should "mapN with errors" in {
    import cats.data.Validated
    import cats.data.Validated.Invalid

    val v1 = Validated.invalidNec[String, Int]("error 1")
    val v2 = Validated.invalidNec[String, Int]("error 2")
    val v3 = Validated.validNec[String, Int](30)

    val r = (v1, v2, v3).mapN(_ * _ * _)
    r shouldEqual Invalid(NonEmptyChain("error 1", "error 2"))
  }
}
