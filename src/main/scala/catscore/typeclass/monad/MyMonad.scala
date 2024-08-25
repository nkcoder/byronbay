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

  given listMonad: Monad[List] with {
    override def pure[A](x: A): List[A] = List(x)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa match
        case Nil    => Nil
        case h :: t => f(h) ++ flatMap(t)(f)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: List[List[A]]): List[A] = flatMap(ffa)(identity)

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???

  }

  def eitherMonad[E]: Monad[[A] =>> Either[E, A]] = new Monad[[A] =>> Either[E, A]] {

    def pure[A](a: A): Either[E, A] = Right(a)

    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }

    override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: Either[E, Either[E, A]]): Either[E, A] = flatMap(ffa)(identity)

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = {
      @annotation.tailrec
      def loop(a: A): Either[E, B] = f(a) match {
        case Left(e)         => Left(e)
        case Right(Left(a1)) => loop(a1)
        case Right(Right(b)) => Right(b)
      }

      loop(a)
    }
  }
}
