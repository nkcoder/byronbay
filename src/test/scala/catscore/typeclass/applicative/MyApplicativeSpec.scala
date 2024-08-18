package my.playground
package catscore.typeclass.applicative

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyApplicativeSpec extends AnyFlatSpec with Matchers {
  behavior of "Applicative => Option"
  it should "ap" in {
    import cats.Applicative

    val option = Applicative[Option].ap(Option((a: Int) => a + 1))(Option(10))
    option shouldEqual Some(11)
  }
  it should "map2" in {
    import cats.syntax.all.*

    val o1: Option[Int] = Some(10)
    val o2: Option[Int] = Some(10)
    val o3: Option[Int] = None
    o1.map2[Int, Int](o2)((x, y) => x + y) shouldEqual Some(20)
  }

  behavior of "MyApplicative -> Option"
  it should "map2" in {
    import catscore.typeclass.applicative.MyApplicative.given
    optionApplicative.map2(Some(10), Some(20))(_ + _) shouldEqual Some(30)
  }
  it should "ap" in {
    import catscore.typeclass.applicative.MyApplicative.given
    optionApplicative.ap(Some((a: Int) => a + 1))(Some(10)) shouldEqual Some(11)
  }

  behavior of "MyApplicative -> List"
  it should "ap" in {
    import catscore.typeclass.applicative.MyApplicative.given
    listApplicative.ap[Int, Int](List((a: Int) => a + 1))(List(10)) shouldEqual List(11)
  }
  it should "map2" in {
    import catscore.typeclass.applicative.MyApplicative.given
    listApplicative.map2[Int, Int, Int](List(100), List(10))(_ + _) shouldEqual List(110)
  }

}
