package my.playground
package catscore.typeclass.monad

import cats.Monad

/**
 * Monad extends the Applicative type class with a new function flatten.
 * Flatten takes a value in a nested context (eg. F[F[A]] where F is the context) and "joins" the contexts together
 * so that we have a single context (ie. F[A]).
 */
class MyMonad

object MyMonad {
  given optionMonad: Monad[Option] with {
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None    => None
    }

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: Option[Option[A]]): Option[A] = flatMap(ffa)(identity)

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(Right(b))   => Some(b)
      case Some(Left(next)) => tailRecM(next)(f)
      case None             => None
    }
  }
}
