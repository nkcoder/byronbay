package my.playground
package catscore.typeclass.warmup

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

object Eq {
  def apply[A](using eq: Eq[A]): Eq[A] = eq

  def instance[A](f: (A, A) => Boolean): Eq[A] = (x: A, y: A) => f(x, y)

  given Eq[Int] with {
    override def eqv(x: Int, y: Int): Boolean = x == y
  }

  given Eq[String] with {
    override def eqv(x: String, y: String): Boolean = x.equals(y)
  }

  /**
   * Use a variable if you need types and implicit parameters.
   */
  given personEq(using intEq: Eq[Int], stringEq: Eq[String]): Eq[Person] with {
    override def eqv(x: Person, y: Person): Boolean =
      intEq.eqv(x.id, y.id) && stringEq.eqv(x.name, y.name)
  }

  /**
   * Automatically derive instances for Eq[Option[A]] given that we have an implicit instance for Eq[A]
   */
  given optionEq[A](using eq: Eq[A]): Eq[Option[A]] with {
    override def eqv(x: Option[A], y: Option[A]): Boolean =
      x.exists(x => y.exists(y => eq.eqv(x, y)))
  }

  object Syntax {
    extension [A](s: A)(using eq: Eq[A]) {
      def eqTo(other: A): Boolean = eq.eqv(s, other)
    }
  }

}
