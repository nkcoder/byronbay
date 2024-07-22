package my.playground
package catscore.typeclass.warmup

class EqSpec extends MySpec {
  checkAll("Eq[Int]", EqTests[Int].eq)
  checkAll("Eq[String]", EqTests[String].eq)
  checkAll("Eq[Person]", EqTests[Person].eq)
}
