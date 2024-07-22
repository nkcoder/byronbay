package my.playground
package catscore.typeclass.warmup

case class Person(id: Int, name: String)

object Person {
  object Instances {
    given eqPersonName(using eqString: Eq[String]): Eq[Person] =
      Eq.instance[Person]((x, y) => eqString.eqv(x.name, y.name))

    given eqPersonId(using eqInt: Eq[Int]): Eq[Person] = Eq.instance[Person]((x, y) => eqInt.eqv(x.id, y.id))
  }
}
