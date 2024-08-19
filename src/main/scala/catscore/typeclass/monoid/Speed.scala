package my.playground
package catscore.typeclass.monoid

import cats.Monoid

/**
 * Monoid: extends the power of Semigroup by providing an additional empty value.
 * [[https://typelevel.org/cats/typeclasses/monoid.html]]
 * {{{
 * trait Semigroup[A] {
 *   def combine(x: A, y: A): A
 * }
 *
 * trait Monoid[A] extends Semigroup[A] {
 *   def empty: A
 * }
 * }}}
 */

case class Speed(value: Double)

object Speed {
  private def addSpeed(s1: Speed, s2: Speed): Speed = Speed(s1.value + s2.value)

  // explicit version
  given monoidSpeed2: Monoid[Speed] with {
    override def empty: Speed                       = Speed(0.0)
    override def combine(x: Speed, y: Speed): Speed = addSpeed(x, y)
  }

  // the same as above but using Monoid.instance
  given monoidSpeed: Monoid[Speed] = Monoid.instance(Speed(0.0), addSpeed)
}

object MonoidExercise {
  given sumMonoid0: Monoid[Int] with {
    override def empty: Int                   = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

  given sumMonoid: Monoid[Int] = Monoid.instance(0, _ + _)

  given minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, _ min _)

  given listMonoid[A]: Monoid[List[A]] = Monoid.instance(List.empty[A], _ ++ _)

  given stringMonoid: Monoid[String] = Monoid.instance("", _ + _)
}
