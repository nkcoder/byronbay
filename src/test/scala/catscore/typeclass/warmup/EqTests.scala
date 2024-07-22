package my.playground
package catscore.typeclass.warmup

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait EqTests[A] extends Laws {
  def laws: EqLaws[A]

  def eq(using arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
    name = "Eq",
    parent = None,
    "reflexivity"  -> forAll(laws.reflexivity(_)),
    "symmetry"     -> forAll((x, y) => laws.symmetry(x, y)),
    "transitivity" -> forAll((x, y, z) => laws.transitivity(x, y, z))
  )

}

object EqTests {
  def apply[A](using eqv: Eq[A]): EqTests[A] = new EqTests[A] {
    def laws: EqLaws[A] = new EqLaws[A] {
      override def eq: Eq[A] = eqv
    }
  }
}
