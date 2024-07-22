package my.playground
package catscore.typeclass.monoid

import cats.kernel.Monoid
import cats.Eq
import org.scalacheck.Prop.{False, True}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpeedMonoidSpec extends AnyFlatSpec with Matchers {

  /**
   * Speed Monoid
   */
  behavior of "Speed Monoid"

  it should "have a Monoid instance" in {
    import catscore.typeclass.monoid.Speed.*

    import cats.implicits.*

    val speed1   = Speed(10.0)
    val speed2   = Speed(20.0)
    val expected = Speed(30.0)

    Monoid[Speed].combine(speed1, speed2) shouldEqual expected

    // |+| is an alias for combine
    (speed1 |+| speed2) shouldEqual expected
  }

  it should "have an empty value" in {
    import catscore.typeclass.monoid.Speed.*

    Monoid[Speed].empty shouldEqual Speed(0.0)

    Monoid[Speed].combine(Speed(10.0), Monoid[Speed].empty) shouldEqual Speed(10.0)
  }

  it should "combine a list of Speed values" in {
    import catscore.typeclass.monoid.Speed.*

    import cats.implicits.given

    val speeds   = List(Speed(10.0), Speed(20.0), Speed(30.0))
    val expected = Speed(60.0)

    speeds.combineAll shouldEqual expected
    Monoid[Speed].combineAll(speeds) shouldEqual expected
  }

  it should "check empty instance" in {
    import catscore.typeclass.monoid.Speed.*

    given speedEq: Eq[Speed] = Eq.fromUniversalEquals

    Monoid[Speed].isEmpty(Speed(10.0)) shouldBe false
    Monoid[Speed].isEmpty(Speed(0.0)) shouldBe true
  }

  /**
   * MonoidExercise
   */
  behavior of "MonoidExercise"

  it should "sum integers" in {

    MonoidExercise.sumMonoid0.combine(10, 20) shouldBe 30
    MonoidExercise.sumMonoid.combine(10, Monoid[Int].empty) shouldBe 10
  }

  it should "find the minimum integer" in {

    MonoidExercise.minMonoid.combine(10, 20) shouldBe 10
    MonoidExercise.minMonoid.combine(20, Monoid[Int].empty) shouldBe 0
  }

  it should "combine lists" in {

    MonoidExercise.listMonoid.combine(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
    MonoidExercise.listMonoid.combine(List.empty[Int], List(1, 2)) shouldBe List(1, 2)
    MonoidExercise.listMonoid[Boolean].combine(List(true), List(false)) shouldBe List(true, false)
  }

  it should "combine strings" in {

    MonoidExercise.stringMonoid.combine("Hello", " World") shouldBe "Hello World"
    MonoidExercise.stringMonoid.combine("Hello", Monoid[String].empty) shouldBe "Hello"
  }

}
