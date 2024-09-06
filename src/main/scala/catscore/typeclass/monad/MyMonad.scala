package my.playground
package catscore.typeclass.monad

import scala.util.{Success, Try, Failure}

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

  /**
   * The `=>>` operator:
   * `=>>` is a type lambda operator introduced in Scala 3. It's used to define anonymous type constructors.
   * In [R] =>> Either[L, R]:
   *
   * This creates a type constructor that takes one type parameter (R).
   * It then applies this R to Either[L, R].
   * The result is a type constructor that only needs one type parameter (R) instead of two (L and R).
   *
   * This is necessary because Monad expects a type constructor of kind * -> * (taking one type parameter), but Either
   * normally takes two type parameters. The =>> syntax allows us to partially apply the L type, leaving a type
   * constructor that only needs the R type.
   *
   * In essence, [R] =>> Either[L, R] is saying: "For any type R, construct an Either with L as the left type and R as
   * the right type." This allows us to treat Either as a Monad for any fixed left type L.
   */
  given eitherMonad[L]: Monad[[R] =>> Either[L, R]] with {
    def pure[A](a: A): Either[L, A] = Right(a)

    def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] = fa match {
      case Right(r) => f(r)
      case Left(l)  => Left(l)
    }

    override def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: Either[L, Either[L, A]]): Either[L, A] = flatMap(ffa)(identity)

    override def tailRecM[A, B](a: A)(f: A => Either[L, Either[A, B]]): Either[L, B] = {
      @annotation.tailrec
      def loop(a: A): Either[L, B] = f(a) match {
        case Left(e)         => Left(e)
        case Right(Left(a1)) => loop(a1)
        case Right(Right(b)) => Right(b)
      }

      loop(a)
    }
  }


  given tryMonad: Monad[Try] with {
    def pure[A](a: A): Try[A] = Success(a)

    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa match {
      case Success(a) => f(a)
      case Failure(e) => Failure(e)
    }

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: Try[Try[A]]): Try[A] = flatMap(ffa)(identity)

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = {
      @annotation.tailrec
      def loop(a: A): Try[B] = f(a) match {
        case Failure(e)           => Failure(e)
        case Success(Left(a1))    => loop(a1)
        case Success(Right(b))    => Success(b)
      }

      loop(a)
    }
  }

}
