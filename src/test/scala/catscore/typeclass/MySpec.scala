package my.playground
package catscore.typeclass

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class MySpec extends AnyFunSuite with FunSuiteDiscipline with Configuration {
  given arbPerson: Arbitrary[Person] = Arbitrary {
    for {
      id   <- Gen.chooseNum(1, 1000)
      name <- Gen.alphaNumStr
    } yield Person(id, name)
  }

}
