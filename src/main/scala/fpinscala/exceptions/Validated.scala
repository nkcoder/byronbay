package my.playground
package fpinscala.exceptions
import fpinscala.exceptions.Either.{Left, Right}

enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(errors: List[E])

  def toEither: Either[List[E], A] = this match
    case Valid(v)   => Right(v)
    case Invalid(e) => Left(e)

  def map[B](f: A => B): Validated[E, B] = this match
    case Valid(v)   => Valid(f(v))
    case Invalid(e) => Invalid(e)

  def map2[EE >: E, B, C](b: Validated[EE, B])(f: (A, B) => C): Validated[EE, C] =
    (this, b) match
      case (Valid(aa), Valid(bb))     => Valid(f(aa, bb))
      case (Invalid(e1), Valid(_))    => Invalid(e1)
      case (Valid(_), Invalid(e2))    => Invalid(e2)
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)

object Validated:
  def fromEither[E, A](e: Either[List[E], A]): Validated[E, A] = e match
    case Right(v) => Valid(v)
    case Left(e)  => Invalid(e)

  def traverse[E, A, B](as: List[A], f: A => Validated[E, B]): Validated[E, List[B]] =
    as.foldRight[Validated[E, List[B]]](Valid(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[E, A](vs: List[Validated[E, A]]): Validated[E, List[A]] =
    traverse(vs, identity)
