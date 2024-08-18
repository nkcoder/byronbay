package my.playground
package catscore.typeclass.applicative

import cats.Applicative

/**
 * Applicative: extends Functor with an ap and pure method.
 * [[https://typelevel.org/cats/typeclasses/applicative.html]]
 *
 * {{{
 * import cats.Functor
 *
 * trait Applicative[F[_]] extends Functor[F] {
 *   def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
 *
 *   def pure[A](a: A): F[A]
 *
 *   def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
 * }
 * }}}
 */

sealed trait MyValidated[+A]

object MyValidated {
  final case class Valid[+A](a: A)               extends MyValidated[A]
  final case class Invalid(errors: List[String]) extends MyValidated[Nothing]

  given validatedApplicative: Applicative[MyValidated] with {
    override def pure[A](a: A): MyValidated[A] = Valid(a)

    override def ap[A, B](ff: MyValidated[A => B])(fa: MyValidated[A]): MyValidated[B] = (ff, fa) match {
      case (Valid(f), Valid(a))       => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
      case (Invalid(e), _)            => Invalid(e)
      case (_, Invalid(e))            => Invalid(e)
    }

    // Using map2 to implement ap
    def ap_v2[A, B](ff: MyValidated[A => B])(fa: MyValidated[A]): MyValidated[B] =
      map2(ff, fa)((f, a) => f(a))

    override def map2[A, B, Z](fa: MyValidated[A], fb: MyValidated[B])(f: (A, B) => Z): MyValidated[Z] =
      (fa, fb) match {
        case (Valid(a), Valid(b))       => Valid(f(a, b))
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
        case (Invalid(e), _)            => Invalid(e)
        case (_, Invalid(e))            => Invalid(e)
      }

    // Using ap and pure to implement map2
    def map2_v2[A, B, Z](fa: MyValidated[A], fb: MyValidated[B])(f: (A, B) => Z): MyValidated[Z] =
      ap(ap(pure(f.curried))(fa))(fb)

    /**
     * Using ap and map to implement map2, check the implementation of map:
     * {{{
     *   override def map[A, B](fa: F[A])(f: A => B): F[B] =
     *     ap(pure(f))(fa)
     * }}}
     */
    def map2_v3[A, B, Z](fa: MyValidated[A], fb: MyValidated[B])(f: (A, B) => Z): MyValidated[Z] =
      ap(map(fa)(f.curried))(fb)

    def tupled[A, B](fa: MyValidated[A], fb: MyValidated[B]): MyValidated[(A, B)] =
      map2(fa, fb)((_, _))
  }
}
